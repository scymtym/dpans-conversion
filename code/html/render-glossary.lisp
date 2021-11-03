(cl:in-package #:dpans-conversion.html)

(define-render (:gentry anchor)
  (let ((term (node-name node)))
    (format t "~V@TProcessing glossary entry ~A~%"
            (* 2 2 #+no (length file-stack)) term)
    (p* "glossary-entry" anchor
        (lambda ()
          (span "term" (a:curry #'recurse '(:name . 1)))
          (recurse '(:body . *))))))
