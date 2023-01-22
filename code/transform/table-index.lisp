(cl:in-package #:dpans-conversion.transform)

(defclass table-index (builder-mixin) ; TODO rename to figure-index
  ((%tables :reader   tables ; TODO could reuse reference table of `build-references'
            :initform (make-hash-table :test #'equal))))

(defmethod transform-node ((transform table-index)
                           recurse relation relation-args node kind relations &key)
  (funcall recurse))

;;; Table collection

(defmethod transform-node ((transform table-index)
                           recurse relation relation-args node (kind (eql :figure)) relations
                           &key label)
  (let* ((builder      (builder transform))
         (caption-node (bp:node-relation builder '(:caption . 1) node))
         (caption      (when caption-node
                         (to-string builder caption-node)))
         (key          (or label caption)))
    (setf (gethash key (tables transform)) caption-node))
  (funcall recurse))

;;; Index generation

(defun table-list (builder tables)
  (let ((items '()))
    (maphash
     (lambda (key title)
       (let ((reference (bp:node (builder :unresolved-reference :namespace :figure ; TODO better use the table objects
                                                     )
                          (1 (:target . 1) (bp:node (builder :chunk :content key)))
                          (1 (:title  . 1) title))))
         (push (bp:node (builder :list-item)
                 (1 (:body . *) reference))
               items)))
     tables)
    (bp:node (builder :item-list)
      (* (:element . *) (nreverse items)))))

(defmethod transform-node ((transform table-index)
                           recurse relation relation-args
                           node (kind (eql :collection)) relations
                           &key)
  ;; Traverse all nodes to collect tables.
  (funcall recurse)
  ;; Build indices.
  (let* ((builder     (builder transform))
         (elements    (bp:node-relation builder '(:element . *) node))
         (table-list  (table-list builder (tables transform)))
         (table-index (bp:node (builder :output-file :filename "figure-index"
                                                     :title    "Figure Index")
                        (1 (:element . 1) (bp:node (builder :section :level 1)
                                            (1 (:name    . 1) (bp:node (builder :chunk :content "Figure Index")))
                                            (1 (:element . *) table-list)))))
         (new-elements (append elements (list table-index))))
    (bp:node (builder :collection)      ; TODO use reconstitute
      (* (:element . *) new-elements))))
