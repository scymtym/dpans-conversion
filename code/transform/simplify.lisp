(cl:in-package #:dpans-conversion.transform)

;;; `simplify'
;;;
;;; TODO

(defclass simplify (default-reconstitute-mixin
                    builder-mixin)
  ())

(labels ((maybe-merge-chunks (builder nodes)
           (let ((last              (a:lastcar nodes))
                 (content-fragments '()))
             (flet ((flush-chunk ()
                      (when content-fragments
                        (let ((content (apply #'concatenate 'string
                                              (nreverse content-fragments))))
                          (setf content-fragments '())
                          (list (bp:make-node builder :chunk :content content))))))
               (let ((result (a:mappend
                       (lambda (node)
                         (case (bp:node-kind builder node)
                           (:chunk
                            (let ((initargs (bp:node-initargs builder node)))
                              (cond ((typep initargs '(cons (eql :content)
                                                            (cons string null)))
                                     (push (second initargs) content-fragments)
                                     (if (eq node last)
                                         (flush-chunk)
                                         '()))
                                    (t
                                     (append (flush-chunk) (list node))))))
                           (t
                            (append (flush-chunk) (list node)))))
                       nodes)))
                 result))))
         (simplify-elements (builder recurse normal-thunk)
           (let* ((maybe-elements (first (funcall recurse :relations '((:element . *)))))
                  (elements       (maybe-merge-chunks
                                   builder (remove nil maybe-elements))))
             (cond ((null elements)        nil)
                   ((a:length= 1 elements) (first elements))
                   (t                      (funcall normal-thunk))))))

  (defmethod transform-node ((transform simplify) recurse
                             relation relation-args node kind relations
                             &rest initargs &key)
    (let ((builder (builder transform)))
      (labels ((maybe-simplify-nodes (nodes)
                 ;; For NODES of kind `:splice', collect all `:element'
                 ;; children into the flat list and drop the `:splice'
                 ;; node.
                 (a:mappend
                  (lambda (node)
                    (case (bp:node-kind builder node)
                      (:splice
                       (bp:node-relation builder '(:element . *) node))
                      (t
                       (list node))))
                  nodes))
               (one-relation (relation)
                 ;; Simplify one relation: recurse, drop nodes that
                 ;; became `nil' in the recursive step, simplify if all
                 ;; remaining nodes are of kind `:splice'.
                 (multiple-value-bind (relation* cardinality)
                     (bp:normalize-relation relation)
                   (declare (ignore relation*))
                   (let ((nodes (first (funcall recurse :relations (list relation)))))
                     (list cardinality relation
                           (cond ((eq cardinality '*)
                                  (maybe-merge-chunks
                                   builder (maybe-simplify-nodes
                                            (remove nil nodes))))
                                 (t
                                  nodes)))))))
        (apply #'reconstitute builder recurse kind
               (mapcar #'one-relation relations)
               initargs))))

  (defmethod transform-node ((transform simplify) recurse
                             relation relation-args node (kind (eql :splice)) relations
                             &key)
    (let ((builder (builder transform)))
      (simplify-elements builder recurse #'call-next-method)))

  (defmethod transform-node ((transform simplify) recurse
                             relation relation-args node (kind (eql :block)) relations
                             &key)
    (let ((builder (builder transform)))
      (simplify-elements builder recurse #'call-next-method))))

#+no (let ((result (apply-transform
               (make-instance 'simplify :builder 'list)
               (bp:with-builder ('list)
                 (bp:node* (:foo)
                   (* (:bar . *) (list (bp:node* (:splice)
                                         (* (:element . *) (list (bp:node* (:chunk :content "foo"))
                                                                 (bp:node* (:chunk :content "bar")))))
                                       (bp:node* (:splice)
                                         (* (:element . *) (list (bp:node* (:splice)
                                                                   (* (:element . *) (list (bp:node* (:chunk :content "baz"))
                                                                                           (bp:node* (:chunk :content "fez")))))
                                                                 (bp:node* (:splice)
                                                                   (* (:element . *) (list (bp:node* (:chunk :content "whoop"))
                                                                                           (bp:node* (:chunk :content "fi")))))))))))))))
  (fresh-line)
  (architecture.builder-protocol.print-tree:serialize
   'list result *standard-output*))
