(cl:in-package #:dpans-conversion.html)

(define-render (:dash which)
  (cxml:unescaped (ecase which
                    (:en "&ndash;")
                    (:em "&mdash;"))))
