(cl:in-package #:dpans-conversion.html)

(define-render (:non-breaking-space)
  (cxml:unescaped "&nbsp;")) ; TODO

(define-render (:paragraph-break)
  (br))

(define-render (:dash which)
  (cxml:unescaped (ecase which
                    (:en "&ndash;")
                    (:em "&mdash;"))))
