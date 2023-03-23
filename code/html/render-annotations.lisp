(cl:in-package #:dpans-conversion.html)

(define-render (:issue-annotation target)
  (if (render-annotation? transform :issue)
      (let ((builder (transform:builder transform)))
        (labels ((in-line? (node)
                   (member (bp:node-kind builder node)
                           '(:chunk :reference :unresolved-reference :ftype)))
                 (content ()
                   (if target
                       (link transform node target :issue
                             (a:curry #'recurse '(:title . 1)))
                       (broken-link #'recurse :issue))
                   (recurse '(:element . *))))
          (if (every #'in-line? (bp:node-relation builder '(:element . *) node))
              (span "issue-annotation" #'content)
              (div "issue-annotation" #'content))))
      (funcall recurse :relations '((:element . *)))))

(define-render (:editor-note editor content anchor)
  (when (render-annotation? transform :editor-note)
    (tooltip "editor-note" "editor-note-tooltip"
             (lambda ()
               (span "editor" editor)
               (cxml:text ": ")
               (cxml:text content))
             "‣"
             :element (lambda (class contination)
                        (span* class anchor contination)))))

(define-render (:reviewer-note (reviewer nil) content anchor)
  (when (render-annotation? transform :reviewer-note)
    (tooltip "reviewer-note" "reviewer-note-tooltip"
             (lambda ()
               (when reviewer
                 (span "reviewer" reviewer)
                 (cxml:text ": "))
               (cxml:text content))
             "‣"
             :element (lambda (class contination)
                        (span* class anchor contination)))))
