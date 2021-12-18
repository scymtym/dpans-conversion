(cl:in-package #:dpans-conversion.transform)

;;; `cleanup-bnf-rules'
;;;
;;; TODO

(defclass cleanup-bnf-rules (default-reconstitute-mixin
                             builder-mixin)
  ())

(defmethod transform-node ((transform cleanup-bnf-rules) recurse
                           relation relation-args node kind relations
                           &rest initargs &key)
  (let ((builder (builder transform)))
    (cond ((not (member '(:element . *) relations :test  #'equal))
           (call-next-method))
          ((notany (lambda (node)
                     (eq (bp:node-kind builder node) :bnf-rule))
                   (bp:node-relation builder '(:element . *) node))
           (call-next-method))
          (t
           (let ((elements  '())
                 (bnf-rules '()))
             (flet ((flush ()
                      (when bnf-rules
                        (push (bp:node (builder :bnf-grammar)
                                (* (:element . *) (nreverse bnf-rules)))
                              elements)
                        (setf bnf-rules '()))))
               (labels ((visit (node)
                          (let ((kind (bp:node-kind builder node)))
                            (cond ((eq kind :bnf-rule) ; Collect BNF rules
                                   (push node bnf-rules))
                                  ;; Whitespace does not terminate the grammar.
                                  ((or (eq kind :paragraph-break)
                                       (and (eq kind :chunk)
                                            (every (a:rcurry #'member '(#\Space #\Tab #\Newline))
                                                   (getf (bp:node-initargs builder node) :content))))
                                   ;; Ignore whitespace between rules but not
                                   ;; before the first rule.
                                   (when (null bnf-rules)
                                     (push node elements)))
                                  ((eq kind :issue-annotation)
                                   (let ((old bnf-rules))
                                     (map nil #'visit (bp:node-relation builder '(:element . *) node))
                                     (cond ((eq old bnf-rules)
                                            (flush)
                                            (push node elements))
                                           (t
                                            (setf bnf-rules (list* node old))))))
                                  ;; Terminate grammar for everything else
                                  (t
                                   (flush)
                                   (push node elements))))))
                 (map nil #'visit (bp:node-relation builder '(:element . *) node)))
               (flush))
             (let* ((other-relations (remove '(:element . *) relations
                                             :test #'equal))
                    (new-node        (apply #'%reconstitute
                                            builder recurse kind other-relations
                                            initargs))
                    (relation        (list '* '(:element . *) (nreverse elements))))
               (bp:finish-node
                builder kind (bp:add-relations
                              builder new-node (list relation)))))))))
