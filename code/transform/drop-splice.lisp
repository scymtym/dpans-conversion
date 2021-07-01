(cl:in-package #:dpans-conversion.transform)

(defclass minimize (default-reconstitute-mixin
                    builder-mixin)
  ())

(defun apply-splices (builder related)
  (loop :for one-related :in related
        :if (member (bp:node-kind builder one-related) '(:splice :block)) ; TODO should there even be blocks at this point?
        :appending (bp:node-relation builder '(:element . *) one-related)
        :else
        :appending (list one-related)))

(defun merge-chunks (builder related)
  related)

(defun minimize-relation/* (builder related)
  (let* ((flat   (apply-splices builder related))
         (merged (merge-chunks builder flat)))
    merged))

(defun minimize-relation/1 (builder related)
  (cond ((not (eq (bp:node-kind builder related) :splice))
         related)
        ((a:length= 1 (bp:node-relation builder '(:element . *) related))
         (break "~A ~A" related (bp:node-relation builder '(:element . *) related)))
        (t
         related)))

(defmethod transform-node ((transform minimize) recurse
                           relation relation-args node (kind t) relations
                           &rest initargs)
  (let* ((builder (builder transform))
         (node    (apply #'bp:make-node builder kind
                         (a:remove-from-plist initargs :bounds :output-file))))
    (bp:add-relations
     builder node
     (map 'list (lambda (relation)
                  (multiple-value-bind (relation* cardinality)
                      (bp:normalize-relation relation)
                    (declare (ignore relation*))
                    (let* ((right     (first (funcall recurse :relations (list relation))))
                           (minimized (ecase cardinality
                                        (*        (minimize-relation/* builder right))
                                        ((1 bp:?) (minimize-relation/1 builder right)))))
                      (list cardinality relation minimized))))
          relations))))
