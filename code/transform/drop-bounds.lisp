(cl:in-package #:dpans-conversion.transform)

;;; `drop-bounds'
;;;
;;; Remove the `:bounds' initarg from all nodes. Should be performed
;;; before `simplify' so that the presence of `:bounds' initargs does
;;; not prevent nodes from being merged.

(defclass drop-bounds (builder-mixin)
  ())

(defmethod transform-node ((transform drop-bounds) recurse
                           relation relation-args node kind relations
                           &rest initargs &key)
  (let ((builder (builder transform)))
    (apply #'reconstitute builder recurse kind relations
           (a:remove-from-plist initargs :bounds))))
