(cl:in-package #:dpans-conversion.transform)

;;; Transform `note-parents'
;;;
;;; For each non-root node, stores its parent in the `:parent'
;;; initarg.

(defclass note-parents (default-reconstitute-mixin
                        builder-mixin)
  ((%stack :accessor stack
           :initform '())))

(defmethod transform-node ((transform note-parents)
                           recurse relation relation-args node kind relations
                           &rest initargs &key)
  (let* ((builder (builder transform))
         (parent  (first (stack transform)))
         (node    (apply #'bp:make-node builder kind
                         (if parent
                             (list* :parent (make-reference parent :hint (list :parent :of kind)) initargs)
                             initargs))))
    (push node (stack transform))
    (unwind-protect
         (bp:add-relations
          builder node
          (map 'list (lambda (relation)
                       (multiple-value-bind (relation* cardinality)
                           (bp:normalize-relation relation)
                         (declare (ignore relation*))
                         (let ((right (first (funcall recurse :relations (list relation)))))
                           (list cardinality relation right))))
               relations))
      (pop (stack transform)))))
