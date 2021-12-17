(cl:in-package #:dpans-conversion.html)

;;; Word level

(define-render (:non-breaking-space)
  (cxml:unescaped "&nbsp;")) ; TODO

(define-render (:dash which)
  (cxml:unescaped (ecase which
                    (:en "&ndash;")
                    (:em "&mdash;"))))

;;; Span level

(define-render (:bold)
  (span "explicit-bold" #'recurse))

(define-render (:italic)
  (span "explicit-italic" #'recurse))

(define-render (:typewriter)
  (span "explicit-mono" #'recurse))

(define-render (:roman)
  (span "explicit-roman" #'recurse))

;;; Paragraph level

(define-render (:hrule)
 (cxml:with-element "hr" (cxml:text " ")))

(define-render (:paragraph-break)
  (br))

(define-render (:newline)
  (br))
