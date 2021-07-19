(cl:in-package #:dpans-conversion.html)

;;; Word level

(define-render (:non-breaking-space)
  (cxml:unescaped "&nbsp;")) ; TODO

(define-render (:dash which)
  (cxml:unescaped (ecase which
                    (:en "&ndash;")
                    (:em "&mdash;"))))

;;; Paragraph level

(define-render (:paragraph-break)
  (br))
