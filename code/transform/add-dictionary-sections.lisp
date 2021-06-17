(cl:in-package #:dpans-conversion.transform)

(defclass add-dictionary-sections (default-reconstitute-mixin
                                   builder-mixin)
  ())

(defmethod transform-node ((transform add-dictionary-sections) recurse
                           relation relation-args node (kind (eql :chapter)) relations
                           &rest initargs &key)
  (let ((builder        (builder transform))
        (components     '())
        (other-elements '()))
    ;; Find components by cheating: Look for the `:input' with
    ;; appropriate names. We do this instead of looking for kind
    ;; `:component' since components can be wrapped in, say,
    ;; `:issue-annotation' nodes.
    (map nil (lambda (child)
               (cond ((and (eq (bp:node-kind builder child) :input)
                           (a:starts-with-subseq
                            "dict-" (getf (bp:node-initargs builder child) :name)))
                      (push child components))
                     (t
                      (push child other-elements))))
         (bp:node-relation builder '(:element . *) node))
    ;; If we did find any component-containing nodes, wrap them in a
    ;; "Dictionary" section and append that section as the final child
    ;; of NODE.
    (if components
        (let* ((other-relations (remove '(:element . *) relations :test #'equal))
               (node            (apply #'%reconstitute builder recurse kind other-relations
                                       initargs))
               (name            (bp:node (builder :chunk :content "Dictionary")))
               (section         (bp:node (builder :section :level  1)
                                  (1 (:name    . 1) name)
                                  (* (:element . *) (nreverse components))))
               (elements        (nreverse (list* section other-elements)))
               (new-node        (bp:add-relations
                                 builder node (list (list '* '(:element . *) elements)))))
          (bp:finish-node builder kind new-node))
        (call-next-method))))
