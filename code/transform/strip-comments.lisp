(cl:in-package #:dpans-conversion.transform)

(defclass strip-comments (default-reconstitute-mixin)
  ())

(defmethod transform-node ((transform strip-comments) recurse
                           relation relation-args node (kind (eql :comment)) relations
                           &key)
  nil)
