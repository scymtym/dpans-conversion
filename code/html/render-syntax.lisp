(cl:in-package #:dpans-conversion.html)

(define-render (:bnf-rule)
  (cxml:with-element "tr"
    (cxml:with-element "td"
      (recurse '(:name . 1)))
    (cxml:with-element "td"
      (cxml:text "::="))
    (cxml:with-element "td"
      (recurse '(:element . *)))))

(define-render (:eql-specializer)
  (cxml:text "(eql ")
  (recurse)
  (cxml:text ")"))

(define-render (:param)
  (span "parameter" #'recurse))

(define-render (:call-syntax which)
  (let ((builder (transform:builder transform)))
    (ecase which
      (:special-operator
       (span "special-operator-definition"
             (lambda ()
               (let ((name (bp:node-relation builder '(:name . 1) node)))
                 (render-name-node builder name)
                 (nbsp)
                 (span "lambda-list" (a:curry recurse :relations '((:argument . *))))
                 (nbsp)
                 (cxml:text "→")
                 (nbsp)
                 (if (member :return-value relations :key #'car)
                     (span "return-values" (a:curry recurse :relations '((:return-value . *))))
                     (cxml:text "|")))))
       (br))
      ((:function :accessor) ; TODO accessor should be a table with rows foo | (setf foo)
       (map nil (lambda (name)
                  (span "function-definition"
                        (lambda ()
                          (render-name-node builder name)
                          (nbsp)
                          (span "lambda-list" (a:curry recurse :relations '((:argument . *))))
                          (nbsp)
                          (cxml:text "→")
                          (nbsp)
                          (if (member :return-value relations :key #'car)
                              (span "return-values" (a:curry recurse :relations '((:return-value . *))))
                              (cxml:text "|"))))
                  (br))
            (bp:node-relation builder '(:name . *) node))
       (when (eq which :accessor)
         (map nil (lambda (name)
                    (span "function-definition"
                          (lambda ()
                            (cxml:text "(setf (")
                            (render-name-node builder name)
                            (nbsp)
                            (span "lambda-list" (a:curry recurse :relations '((:argument . *))))
                            (cxml:text ")")
                            (nbsp)
                            (span "new-value" (a:curry recurse :relations '((:new-value . 1))))))
                    (br))
              (bp:node-relation builder '(:name . *) node))))
      (:generic-function
       (span "function-definition"
             (lambda ()
               (let ((name (bp:node-relation builder '(:name . 1) node)))
                 (render-name-node builder name)
                 (nbsp)
                 (span "lambda-list"   (a:curry recurse :relations '((:argument . *))))
                 (nbsp)
                 (cxml:text "→")
                 (nbsp)
                 (if (member :return-value relations :key #'car)
                     (span "return-values" (a:curry recurse :relations '((:return-value . *))))
                     (cxml:text "|")))))
       (br))
      (:method
          (span "method"
           (lambda ()
             (let ((name (bp:node-relation builder '(:name . 1) node)))
               (render-name-node builder name)
               (nbsp)
               (span "lambda-list" (a:curry recurse :relations '((:argument . *)))))))
        (br))
      (:macro
       (span "function-definition"
             (lambda ()
               (let ((name (bp:node-relation builder '(:name . 1) node)))
                 (render-name-node builder name)
                 (nbsp)
                 (span "lambda-list" (a:curry recurse :relations '((:argument . *))))
                 (nbsp)
                 (cxml:text "→")
                 (nbsp)
                 (if (member :return-value relations :key #'car)
                     (span "return-values" (a:curry recurse :relations '((:return-value . *))))
                     (cxml:text "|")))))
       (br))
      (:type
       (span "type-definition"
             (lambda ()
               (let ((name (bp:node-relation builder '(:name . 1) node)))
                 (cxml:text "(")
                 (render-name-node builder name)
                 (nbsp)
                 (span "lambda-list" (a:curry recurse :relations '((:element . *)))) ; TODO relation name
                 (cxml:text ")"))))
       (br))
      (:setf
       (map nil (lambda (name)
                  (span "setf-definition"
                        (lambda ()
                          (cxml:text "(setf (") ; TODO
                          (span "name" (lambda ()
                                         (cxml:text (dpans-conversion.transform::evaluate-to-string
                                                     builder name))))
                          (nbsp)
                          (span "lambda-list" (a:curry recurse :relations '((:argument . *))))
                          (cxml:text ") ")
                          (span "new-value" (a:curry recurse :relations '((:new-value . 1))))
                          (cxml:text ")")))
                  (br))
            (bp:node-relation builder '(:name . *) node))))))
