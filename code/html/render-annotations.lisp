(cl:in-package #:dpans-conversion.html)

(define-render (:issue-annotation target)
  (if (render-annotation? transform :issue)
      (let ((builder (transform:builder transform)))
        (labels ((in-line? (node)
                   (case (bp:node-kind builder node)
                     (:chunk     t)
                     (:reference t)
                     (:ftype     t)
                     (t          nil)))
                 (content ()
                   (link transform node target :issue
                         (lambda ()
                           (issue-reference-title
                            builder node target :explicit? t)))
                   (funcall recurse :relations '((:element . *)))))
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
