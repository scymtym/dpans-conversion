(cl:in-package #:dpans-conversion.html)

(define-render (:syntax classes)
  (let ((class (format nil "~{syntax-~(~A~)~^ ~}" (a:ensure-list classes)))) ; TODO should always be a list
    (span class #'recurse)))

(define-render (:listing (in-line? nil))
  (flet ((do-code ()
           (cxml:with-element "code"
             (recurse))))
    (if in-line?
        (do-code)
        (cxml:with-element "pre"
          (do-code)))))
