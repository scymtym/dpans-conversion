(cl:in-package #:dpans-conversion.html)

(define-render (:header) ; TODO specializer on relation (eql :header)
  (cxml:with-element "th" (funcall recurse)))

(define-render (:row)
  (cxml:with-element "tr" (funcall recurse)))

(define-render (:cell (span nil))
  (cxml:with-element "td"
    (when span
      (cxml:attribute "colspan" (princ-to-string span)))
    (funcall recurse)))

(define-render (:table)
  #+no (let* ((id-node (find-child-of-kind builder :define-figure node))
              (id      (when id-node
                         (node-name id-node)))
              (anchor  (format nil "figure-~A" id)))
         (when id (break))
         (cxml:attribute "id" anchor))

  (span "float"
        (lambda ()
          (cxml:with-element "table"
            (when (member '(:header . *) relations :test #'equal)
              (cxml:with-element "thead"
                (funcall recurse :relations '(:header))))
            (cxml:with-element "tbody"
              (funcall recurse :relations '(:row))))
          (when (member '(:caption . 1) relations :test #'equal)
            (div "caption" (lambda () (funcall recurse :relations '((:caption . 1)))))))))
