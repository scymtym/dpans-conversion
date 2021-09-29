(cl:in-package #:dpans-conversion.html)

(define-render (:header) ; TODO specializer on relation (eql :header)
  (cxml:with-element "th" (recurse)))

(define-render (:row)
  (cxml:with-element "tr" (recurse)))

(define-render (:cell (span nil))
  (cxml:with-element "td"
    (when span
      (cxml:attribute "colspan" (princ-to-string span)))
    (recurse)))

(define-render (:table)
  (cxml:with-element "table"
    (when (member '(:header . *) relations :test #'equal)
      (cxml:with-element "thead"
        (recurse '(:header . *))))
    (cxml:with-element "tbody"
      (recurse '(:row . *)))))

(define-render (:figure (anchor nil))
  (span* "float" anchor
         (lambda ()
           (recurse '(:element . *))
           (when (member '(:caption . 1) relations :test #'equal)
             (div "caption" (lambda ()
                              (recurse '(:caption . 1))))))))
