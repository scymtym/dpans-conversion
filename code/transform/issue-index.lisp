(cl:in-package #:dpans-conversion.transform)

(defclass issue-index (builder-mixin)
  ((%issues :reader   issues ; TODO could reuse reference table of `build-references'
            :initform (make-hash-table :test #'equal))))

(defmethod transform-node ((transform issue-index)
                           recurse relation relation-args node kind relations &key)
  (funcall recurse))

(defmethod transform-node ((transform issue-index)
                           recurse relation relation-args
                           node (kind (eql :collection)) relations
                           &rest initargs &key)
  (funcall recurse)
  (let* ((builder      (builder transform))
         (elements     (bp:node-relation builder '(:element . *) node))
         (issues       (let ((issues '()))
                         (maphash (lambda (name issue)
                                    (push (bp:node (builder :issue-reference :name name))
                                          issues))
                                  (issues transform))
                         (sort issues #'string-lessp
                               :key (lambda (issue)
                                      (getf (bp:node-initargs builder issue) :name)))))
         (issue-index  (bp:node (builder :output-file :filename "issue-index"
                                                      :title    "Issue Index")
                         (1 (:element . 1) (bp:node (builder :section :level 1)
                                             (1 (:name    . 1) (bp:node (builder :chunk :content "Issue Index")))
                                             (1 (:element . *) (bp:node (builder :item-list)
                                                                 (* (:element . *) (map 'list (lambda (issue)
                                                                                                (bp:node (builder :list-item)
                                                                                                  (1 (:body . *) issue)))
                                                                                        issues))))))))
         (new-elements (append elements (list issue-index))))
    (bp:node (builder :collection)
      (* (:element . *) new-elements))))

(defmethod transform-node ((transform issue-index)
                           recurse relation relation-args node (kind (eql :issue)) relations &key)
  (let ((name (node-name node)))
    (setf (gethash name (issues transform)) node))
  (funcall recurse))
