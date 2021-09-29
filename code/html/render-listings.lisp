(cl:in-package #:dpans-conversion.html)

(define-render (:syntax classes)
  (let ((class (format nil "~{syntax-~(~A~)~^ ~}" (a:ensure-list classes)))) ; TODO should always be a list
    (span class #'recurse)))

(define-render (:listing)
  (cxml:with-element "pre"
    (cxml:with-element "code"
      (recurse))))
