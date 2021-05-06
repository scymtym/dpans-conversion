(cl:in-package #:dpans-conversion.transform)

(defclass strip-tex-commands (default-reconstitute-mixin)
  ())

(defmethod transform-node ((transform strip-tex-commands) recurse
                           relation relation-args node (kind (eql :other-command-application)) relations
                           &rest initargs)
  nil)
