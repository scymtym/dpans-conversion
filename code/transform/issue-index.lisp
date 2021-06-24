(cl:in-package #:dpans-conversion.transform)

(defclass issue-index (builder-mixin)
  ((%issues :reader   issues ; TODO could reuse reference table of `build-references'
            :initform (make-hash-table :test #'equal))))

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
                      :key (lambda (issue)
                             (getf (bp:node-initargs builder issue) :name)))))
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
                 (push (bp:node (builder :issue-reference :name name))
                       (gethash process groups))))
             (issues transform))
    ;;
    (maphash (lambda (process issues)
               (push (issue-list-section builder process issues)
                     sections))
             groups)
    ;;
    (let* ((elements     (bp:node-relation builder '(:element . *) node))
           (issue-index  (bp:node (builder :output-file :filename "issue-index"
                                                        :title    "Issue Index")
                           (1 (:element . 1) (if (a:length= 1 sections)
                                                 (first sections)
                                                 (bp:node (builder :section :level 1)
                                                   (1 (:name    . 1) (bp:node (builder :chunk :content "Issue Indices")))
                                                   (* (:element . *) sections))))))
           (new-elements (append elements (list issue-index))))
      (bp:node (builder :collection) ; TODO use reconstitute
        (* (:element . *) new-elements)))))
