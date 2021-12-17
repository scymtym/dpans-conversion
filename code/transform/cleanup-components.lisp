(cl:in-package #:dpans-conversion.transform)

;;; `cleanup-components'
;;;
;;; TODO

(defclass cleanup-components (default-reconstitute-mixin
                              builder-mixin)
  ())

(defmethod transform-node ((transform cleanup-components) recurse
                           relation relation-args node (kind (eql :component)) relations
                           &rest initargs &key)
  (let ((builder         (builder transform))
        (other-relations (remove '(:element . *) relations :test #'equal))
        (ftype-element   nil)
        (other-elements  '()))
    ;; Find `:element' child which contains the `:ftype' node. This
    ;; can be an ancestor instead of a child due to, for example,
    ;; issue annotations.
    (map nil (lambda (child)
               (if (find-ancestor-of-kind builder :ftype child)
                   (setf ftype-element child)
                   (push child other-elements)))
         (bp:node-relation builder '(:element . *) node))
    ;; Relate the `:ftype' node via a new `:ftype' relation. Also
    ;; store the ftype as an initarg for convenience.
    (let* ((ftype-node (find-ancestor-of-kind builder :ftype ftype-element))
           (ftype      (node-name ftype-node))
           (new-node   (apply #'%reconstitute builder recurse kind other-relations
                              :ftype ftype initargs))
           (relations  (list (list '1 '(:ftype   . 1) ftype-element)
                             (list '* '(:element . *) (nreverse other-elements))))
           (new-node   (bp:add-relations builder new-node relations)))
      (bp:finish-node builder kind new-node))))
