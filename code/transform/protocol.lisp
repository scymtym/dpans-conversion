(cl:in-package #:dpans-conversion.transform)

(defmethod apply-transform ((transform t) (ast t))
  (let ((builder 'list))
    (labels ((visit (recurse relation relation-args node kind relations &rest initargs)
               (apply #'transform-node transform recurse
                      relation relation-args node kind relations
                      initargs)))
      (bp:walk-nodes builder #'visit ast))))

(defmethod apply-transforms ((transforms sequence) (ast t))
  (reduce (lambda (ast transform)
            (apply-transform transform ast))
          transforms :initial-value ast))
