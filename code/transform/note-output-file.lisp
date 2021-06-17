(cl:in-package #:dpans-conversion.transform)

(defclass note-output-file (output-file-tracking-mixin
                            default-reconstitute-mixin
                            environment-mixin
                            builder-mixin)
  ()
  (:default-initargs
   :environment (make-instance 'env:lexical-environment :parent **meta-environment**)))

(defmethod transform-node ((transform note-output-file) recurse
                           relation relation-args node kind relations
                           &rest initargs &key)
  (a:if-let ((output-file (current-output-file transform)))
    (apply #'reconstitute (builder transform) recurse kind relations
           :output-file output-file
           initargs)
    (call-next-method)))
