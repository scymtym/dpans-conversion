(cl:in-package #:dpans-conversion.transform)

(defclass drop (default-reconstitute-mixin
                builder-mixin)
  ((%predicate :initarg :predicate
               :reader  predicate))
  (:default-initargs
   :predicate (error "Missing required initarg ~S" :predicate)))

(defmethod transform-node ((transform drop) recurse
                           relation relation-args node (kind t) relations
                           &rest initargs)
  (if (apply (predicate transform) relation relation-args node kind relations
             initargs)
      nil
      (call-next-method)))

(defun one-of? (&rest values)
  (lambda (value)
    (member value values :test #'equal)))

(defun kind? (kind)
  (lambda (relation relation-args node kind* relations
           &key &allow-other-keys)
    (declare (ignore relation relation-args node relations))
    (if (functionp kind)
        (funcall kind kind*)
        (eq kind kind*))))

(defun initarg? (initarg value)
  (lambda (relation relation-args node kind relations
           &rest initargs &key &allow-other-keys)
    (declare (ignore relation relation-args kind node relations))
    (let ((value* (getf initargs initarg)))
      (if (functionp value)
          (funcall value value*)
          (equalp value value*)))))

(defun relation? (relation)
  (flet ((relation-matches? (relation* relation-args node kind relations
                             &key &allow-other-keys)
           (declare (ignore relation-args node kind relations))
           (eq relation relation*)))
    (make-instance 'drop :predicate #'relation-matches?)))
