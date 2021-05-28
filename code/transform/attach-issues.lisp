(cl:in-package #:dpans-conversion.transform)

(defclass attach-issue-references (default-reconstitute-mixin)
  ((%active-issues :accessor active-issues
                   :initform '())))

(defmethod transform-node ((transform attach-issue-references) recurse
                           relation relation-args node (kind (eql :issue-annotation)) relations
                           &key &allow-other-keys)
  (let ((builder 'list))
    (let ((name (bp:node-relation builder '(:name . 1) node)))
      (push name (active-issues transform))
      (unwind-protect
           node
           #+no (bp:node (builder :splice)
                  (* (:element . *) (first (funcall recurse :relations '((:element . *))))))
        (pop (active-issues transform))))))

(defmethod transform-node ((transform attach-issue-references) recurse
                           relation relation-args node (kind (eql :component)) relations
                           &rest initargs)
  (let ((builder 'list))
    (a:if-let ((active-issues (active-issues transform)))
      (let ((relations (list* (list '* '(:issue . *) active-issues)
                              relations)))
        (apply #'reconstitute builder recurse kind relations initargs))
      (call-next-method))))
