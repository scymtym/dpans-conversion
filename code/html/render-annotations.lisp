(cl:in-package #:dpans-conversion.html)

(define-render (:issue-annotation target)
  (div "issue-annotation"
       (lambda ()
         (issue-link transform node target :explicit? t)
         (funcall recurse :relations '(:element)))))

(define-render (:editor-note editor content)
  (tooltip "editor-note" "editor-note-tooltip"
           (lambda ()
             (span "editor" (lambda () (cxml:text editor)))
             (cxml:text ": ")
             (cxml:text content))
           (lambda () (cxml:text "‣"))
           :element 'span))

(define-render (:reviewer-note (reviewer nil) content)
  (tooltip "reviewer-note" "reviewer-note-tooltip"
           (lambda ()
             (when reviewer
               (span "reviewer" (lambda () (cxml:text reviewer)))
               (cxml:text ": "))
             (cxml:text content))
           (lambda () (cxml:text "‣"))
           :element 'span))
