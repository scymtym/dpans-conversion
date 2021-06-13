(cl:in-package #:dpans-conversion.html)

(define-render (:issue-annotation name)
  (div "issue-annotation"
       (lambda ()
         (span "issue-reference"
               (lambda ()
                 (let* ((index  (position #\: name))
                        (issue  (if index
                                    (subseq name 0 index)
                                    name))
                        (target (format nil "issues/~(~A~).html" issue)))
                   (a target (lambda ()
                               (cxml:text "Issue: ")
                               (cxml:text name))))))
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
