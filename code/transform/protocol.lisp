(cl:in-package #:dpans-conversion.transform)

;;;

(defgeneric apply-transfrom (transform ast))

(defgeneric peek-node (transfrom builder relation relation-args node kind))

(defgeneric transform-node (transform recurse
                            relation relation-args node kind relations
                            &rest initargs &key &allow-other-keys))

;;;

(defmethod apply-transform ((transform t) (ast t))
  (let ((builder (builder transform)))
    (labels ((visit (recurse relation relation-args node kind relations &rest initargs)
               (apply #'transform-node transform recurse
                      relation relation-args node kind relations
                      initargs)))
      (bp:walk-nodes builder #'visit ast))))

(defmethod apply-transforms ((transforms sequence) (ast t))
  (reduce (lambda (ast transform)
            (apply-transform transform ast))
          transforms :initial-value ast))
