(cl:in-package #:dpans-conversion.html)

(define-render (:issue-annotation target)
  (let ((builder (transform:builder transform)))
    (labels ((in-line? (node)
               (case (bp:node-kind builder node)
                 (:chunk     t)
                 (:reference t)
                 (t          nil)))
             (content ()
               (issue-link transform node target :explicit? t)
               (funcall recurse :relations '((:element . *)))))
      (if (every #'in-line? (bp:node-relation builder '(:element . *) node))
          (span "issue-annotation" #'content)
          (div "issue-annotation" #'content)))))

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
