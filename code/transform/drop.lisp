(cl:in-package #:dpans-conversion.transform)

(defclass drop (default-reconstitute-mixin
                builder-mixin)
  ((%predicate :initarg  :predicate
               :reader   predicate)
   (%action    :initarg  :action
               :type     (member :drop :replace-with-children)
               :reader   action
               :initform :drop))
  (:default-initargs
   :predicate (error "Missing required initarg ~S" :predicate)))

(defmethod transform-node ((transform drop) recurse
                           relation relation-args node (kind t) relations
                           &rest initargs)
  (let ((builder   (builder transform))
        (predicate (predicate transform))
        (action    (action transform)))
    (cond ((not (apply predicate relation relation-args node kind relations
                       initargs))
           (call-next-method))
          ((eq action :drop)
           nil)
          ((eq action :replace-with-children)
           (cond ((null relations)
                  nil)
                 ((typep relations '(cons (cons t (member 1 bp:?)) null))
                  (first (funcall recurse)))
                 ((typep relations '(cons (cons (eql :element) (eql *)) null))
                  (let ((elements (first (funcall recurse))))
                    (if (a:length= 1 elements)
                        (first elements)
                        (bp:node (builder :splice)
                          (* (:element . *) elements)))))
                 (t
                  (break "Cannot handle action ~S for ~S " action node)))))))

;;; Query

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
