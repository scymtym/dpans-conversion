(cl:in-package #:dpans-conversion.transform)

(defclass lower-display-tables (default-reconstitute-mixin
                                environment-mixin
                                builder-mixin)
  ()
  (:default-initargs
   :environment (make-instance 'env:lexical-environment :parent **meta-environment**)))

(defmethod transform-node ((transform lower-display-tables) recurse
                           relation relation-args node (kind (eql :chunk)) relations
                           &key)
  (let ((environment (environment transform)))
    (if (eq (env:lookup :display? :traversal environment
                        :if-does-not-exist nil)
            :cell)
        (let* ((builder (builder transform))
               (name    (to-string builder node)))
          (bp:node (builder :possible-reference :name name)))
        (call-next-method))))

(defmethod transform-node ((transform lower-display-tables) recurse
                           relation relation-args node (kind (eql :cell)) relations
                           &key)
  (let ((environment (environment transform)))
    (if (eq (env:lookup :display? :traversal environment
                        :if-does-not-exist nil)
            :table)
        (call-with-environment
         #'call-next-method
         transform '((:display? . :traversal)) '(:cell))
        (call-next-method))))

(defmethod transform-node ((transform lower-display-tables) recurse
                           relation relation-args node (kind (eql :table)) relations
                           &rest initargs &key which)
  (case which
    (:display
     (let ((builder (builder transform)))
       (call-with-environment
        (lambda ()
          (apply #'reconstitute builder recurse :table relations initargs))
        transform '((:display? . :traversal)) '(:table))))
    (t
     (call-next-method))))
