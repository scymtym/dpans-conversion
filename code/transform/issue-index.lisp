(cl:in-package #:dpans-conversion.transform)

;;; `issue-index'
;;;
;;; Traverse all nodes to find issues

(defclass issue-index (builder-mixin)
  (;; Parameters
   (output-file? :initarg :output-file?
                 :reader  output-file?
                 :documentation
                 "Controls whether the generated index is wrapped in a `:output-file'
                  node.")
   ;; State
   (%issues      :reader   issues ; TODO could reuse reference table of `build-references'
                 :initform (make-hash-table :test #'equal)))
  (:default-initargs
   :output-file? (a:required-argument :output-file?)))

(defmethod transform-node ((transform issue-index)
                           recurse relation relation-args node kind relations &key)
  (funcall recurse))

;;; Issue collection

(defmethod transform-node ((transform issue-index)
                           recurse relation relation-args node (kind (eql :issue)) relations &key)
  (let ((name (node-name node)))
    (setf (gethash name (issues transform)) node))
  (funcall recurse))

;;; Index generation

(defun issue-list (builder issues)
  (let ((sorted (sort (copy-seq issues) #'string-lessp
                      :key (lambda (issue-reference)
                             (let ((target (bp:node-relation
                                            builder '(:target . 1) issue-reference)))
                               (to-string builder target))))))
    (bp:node (builder :item-list)
      (* (:element . *) (map 'list (lambda (issue)
                                     (bp:node (builder :list-item)
                                       (1 (:body . *) issue)))
                             sorted)))))

(defun issue-list-section (builder process issues)
  (let ((title (format nil "~@:(~A~) Issues" process)))
    (bp:node (builder :section :level 2)
      (1 (:name    . 1) (bp:node (builder :chunk :content title)))
      (1 (:element . *) (issue-list builder issues)))))

(defmethod transform-node ((transform issue-index)
                           recurse relation relation-args
                           node (kind (eql :collection)) relations
                           &rest initargs &key)
  ;; Traverse all nodes to collect issues.
  (funcall recurse)
  ;; Build indices.
  (let* ((builder  (builder transform))
         (groups   (make-hash-table :test #'equal))
         (sections '()))
    ;;
    (maphash (lambda (name issue)
               (let ((process (getf (bp:node-initargs builder issue) :process)))
                 (push (bp:node (builder :issue-reference)
                         (1 (:target . 1) (bp:node (builder :chunk :content name))))
                       (gethash process groups))))
             (issues transform))
    ;;
    (maphash (lambda (process issues)
               (push (issue-list-section builder process issues)
                     sections))
             groups)
    ;;
    (let* ((elements     (bp:node-relation builder '(:element . *) node))
           (issue-index  (if (a:length= 1 sections)
                             (first sections)
                             (bp:node (builder :section :level 1)
                               (1 (:name    . 1) (bp:node (builder :chunk :content "Issue Indices")))
                               (* (:element . *) sections))))
           (wrapped      (if (output-file? transform)
                             (bp:node (builder :output-file :filename "issue-index"
                                                            :title    "Issue Index")
                               (1 (:element . 1) issue-index))
                             issue-index))
           (new-elements (append elements (list wrapped)))
           (new-node     (apply #'%reconstitute
                                builder
                                (lambda (&key relations)
                                  (map 'list (lambda (relation)
                                               (bp:node-relation
                                                builder relation node))
                                       relations))
                                kind relations initargs)))
      (bp:finish-node
       builder kind (bp:add-relations
                     builder new-node
                     (list (list '* '(:element . *) new-elements)))))))
