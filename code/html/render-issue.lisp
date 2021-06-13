(cl:in-package #:dpans-conversion.html)

(defun special-section-string (builder relation node)
  (a:when-let ((value (bp:node-relation builder `(,relation . 1) node)))
    (transform::evaluate-to-string builder value)))

(define-render (:issue filename)
  (let* ((builder          (transform:builder transform))
         (output-directory (output-directory transform))
         (filename         (make-pathname :name      (pathname-name filename)
                                          :type      "html"
                                          :directory '(:relative "issues")))
         (title            (let ((string (special-section-string builder :name node)))
                             (format nil "Issue ~A" string))))
    (with-html-document (stream filename output-directory :title title :use-sidebar t)
      (when t                           ; use-sidebar
        (transform:apply-transform
         (make-instance 'navigation-sidebar :builder builder :output-directory output-directory) node))
      (div "content"
           (lambda ()
             (let ((level 2))
               (labels ((visit (context recurse relation relation-args node kind relations
                                &rest initargs &key name content source explicit? &allow-other-keys)
                          (ecase kind
                            (:dash
                             (apply #'transform:transform-node transform recurse relation relation-args node kind relations initargs)
                             (cxml:unescaped "&ndash;"))
                            (:chunk
                             (cxml:text content))
                            (:possible-reference
                             (a name (lambda () (cxml:text name))))
                            (:issue-reference
                             (flet ((format-link ()
                                      (let ((url (format nil "~(~A~).html" name)))
                                        (a url (lambda ()
                                                 (when explicit?
                                                   (cxml:text "Issue "))
                                                 (cxml:text name))))))
                               (case context
                                 ((:related-issues :required-issues)
                                  (cxml:with-element "li" ; TODO should not assume list
                                    (format-link)))
                                 (t
                                  (format-link)))))
                            (:line
                             (funcall recurse)
                             (case context
                               (:code (br))
                               (t     (cxml:text " "))))
                            (:paragraph-break
                             (br))
                            (:section
                             (unwind-protect
                                  (let ((anchor (format nil "section-~A" name)))
                                    (cxml:with-element "section"
                                      (cxml:attribute "id" anchor)
                                      (h level name)
                                      (incf level)
                                      (if (or (eql (search "Example" name) 0)
                                              (eql (search "Test Case" name) 0))
                                          (cxml:with-element "pre"
                                            (funcall recurse :function (a:curry #'visit :code)))
                                          (funcall recurse))))
                               (decf level)))
                            (:preamble)
                            (:issue
                             (let ((string   (special-section-string builder :name node))
                                   (forum    (special-section-string builder :forum node))
                                   (category (special-section-string builder :category node)))
                               (h 1 (format nil "Issue ~A [~A] [~A]" string forum category)))
                             (when (bp:node-relation builder '(:related-issue . *) node)
                               (h 2 "Related issues")
                               (cxml:with-element "ul"
                                 (funcall recurse :relations '(:related-issue)
                                                  :function  (a:curry #'visit :related-issues))))
                             (when (bp:node-relation builder '(:required-issue . *) node)
                               (h 2 "Required issues")
                               (cxml:with-element "ul"
                                 (funcall recurse :relations '(:required-issue)
                                                  :function  (a:curry #'visit :required-issues))))
                             (funcall recurse :relations '(:section))))))
                 (bp:walk-nodes builder (a:curry #'visit t) node))))))))
