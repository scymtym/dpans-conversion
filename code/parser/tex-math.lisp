(cl:in-package #:dpans-conversion.parser)

(in-grammar dpans)

(macrolet ((define (rule-name operator-expression kind &optional left-optional?)
             `(defrule ,rule-name (environment)
                  (bounds (start end)
                    (seq/ws ,(let ((left-expression '(<- left (element environment))))
                               (if left-optional?
                                   `(? ,left-expression)
                                   left-expression))
                            ,operator-expression
                            (<- right (element environment))))
                (bp:node* (,kind :bounds (cons start end))
                  (1 (:left  . *) left)
                  (1 (:right . *) right)))))
  (define over  "\\over"         :over)
  (define sub   (or "_" "\\sub") :subscript)
  (define super "^"              :superscript t))

#+old (defrule over (environment)
    (seq/ws (<- top (element environment))
            "\\over"
            (<- bottom (element environment)))
  (bp:node* (:over)
    (1 (:top    . 1) top)
    (1 (:bottom . 1) bottom)))

#+old (defrule sub (environment)
  (bounds
   (seq/ws (<- left (element environment))
           (or "_" "\\sub")
           (<- right (element environment))))
  (bp:node* (:subscript)
    (1 (:left  . *) left)
    (1 (:right . *) right)))

#+old (defrule super (environment)
    (bounds (start end)
      (seq/ws (? (<- left (element environment)))
              "^"
              (<- right (element environment))))
  (bp:node* (:superscript :bounds (cons start end))
    (bp:? (:left  . *) left)
    (1    (:right . *) right)))

(defrule math-operators (environment)
  (bounds (start end)
    (seq (:transform (seq)
           (unless (and (eq (env:lookup :mode :traversal environment :if-does-not-exist :normal) :math)
                        (not (eql (env:lookup :math-operator :traversal environment :if-does-not-exist nil)
                              start)))
             (:fail)))
         (<- new-environment (:transform (seq)
                               (env:augmented-environment
                                environment '((:math-operator . :traversal)) (list start))))
         (or (over  new-environment)
             (sub   new-environment)
             (super new-environment)))))
