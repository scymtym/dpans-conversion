(cl:in-package #:dpans-conversion.html)

;;; Item list

(define-render (:list-item)
  (cxml:with-element "li"
    (recurse '(:body . *))))

(define-render (:item-list)
  (cxml:with-element "ul" (recurse)))

;;; Enumeration list

(define-render (:enumeration-item)
  (cxml:with-element "li"
    (recurse '(:body . *))))

(define-render (:enumeration-list)
  (cxml:with-element "ol" (recurse)))

;;; Definition list

(define-render (:definition-item)
  (cxml:with-element "dt"
    (recurse '(:key . *)))
  (cxml:with-element "dd"
    (recurse '(:body . *))))

(define-render (:definition-list)
  (cxml:with-element "dl" (recurse)))
