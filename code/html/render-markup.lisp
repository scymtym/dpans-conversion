(cl:in-package #:dpans-conversion.html)

;;; Item list

(define-render (:list-item)
  (cxml:with-element "li"
    (funcall recurse :relations '((:body . *)))))

(define-render (:item-list)
  (cxml:with-element "ul" (funcall recurse)))

;;; Enumeration list

(define-render (:enumeration-item)
  (cxml:with-element "li"
    (funcall recurse :relations '((:body . *)))))

(define-render (:enumeration-list)
  (cxml:with-element "ol" (funcall recurse)))

;;; Definition list

(define-render (:definition-item)
  (cxml:with-element "dt"
    (funcall recurse :relations '(:key)))
  (cxml:with-element "dd"
    (funcall recurse :relations '((:body . *)))))

(define-render (:definition-list)
  (cxml:with-element "dl" (funcall recurse)))
