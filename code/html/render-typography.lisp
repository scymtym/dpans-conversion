(cl:in-package #:dpans-conversion.html)

;;; Character level

(define-render (:subscript)
  (cxml:with-element "sub" (funcall recurse)))

(define-render (:superscript)
  (cxml:with-element "sup" (funcall recurse)))

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
