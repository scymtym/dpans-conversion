(cl:in-package #:dpans-conversion.html)

(defun special-section-string (builder relation node)
  (a:when-let ((value (bp:node-relation builder `(,relation . 1) node)))
    (transform::evaluate-to-string builder value)))

(defun issue-link (transform reference target &key explicit?)
  (let* ((builder  (transform:builder transform))
         (initargs (bp:node-initargs builder reference)))
    (destructuring-bind (&key name proposal (explicit? explicit?)
                         &allow-other-keys)
        initargs
      (labels ((format-issue ()
                 (when explicit?
                   (cxml:text "Issue ")) ; TODO use proposal when linking to proposal?
                 (cxml:text name)
                 (when proposal
                   (cxml:text ":")
                   (cxml:text proposal))))
        (let ((class "issue-reference"))
          (if target
              (let ((url (node-url transform reference target)))
                (a* url class #'format-issue))
              (span (list class "error") #'format-issue)))))))

(define-render (:issue-reference (target nil)) ; TODO should be a :reference
  (case relation
    ((:related-issue :required-issue)
     (cxml:with-element "li"          ; TODO should not assume list
       (issue-link transform node target)))
    (t
     (issue-link transform node target))))

(define-render (:proposal name anchor)
  (cxml:with-element "section"
    (cxml:attribute "class" "proposal")
    (cxml:attribute "id" anchor)
    (h 2 (format nil "Proposal~@[ ~A~]" name))
    (funcall recurse)))

(define-render (:issue filename)
  (let* ((builder          (transform:builder transform))
         #+no (output-directory (output-directory transform))
         #+no (filename         (make-pathname :name      (pathname-name filename)
                                          :type      "html"
                                          :directory '(:relative "issues")))
         #+no (title            (let ((string (special-section-string builder :name node)))
                             (format nil "Issue ~A" string))))

    #+no (with-html-document (stream filename output-directory :title title :use-sidebar t)
      (when t                           ; use-sidebar
        (transform:apply-transform
         (make-instance 'navigation-sidebar :builder builder :output-directory output-directory) node))
      (div "content"
           (lambda ()
             )))

    (let ((level 2))
      (labels ((visit (context recurse relation relation-args node kind relations
                       &rest initargs &key name content source explicit? anchor  &allow-other-keys)
                 (ecase kind
                   ((:dash :reference :issue-reference :proposal :enumeration-list :enumeration-item)
                    (apply #'transform:transform-node transform recurse relation relation-args node kind relations initargs))
                   (:chunk
                    (cxml:text content))
                   (:possible-reference
                    (a name (lambda () (cxml:text name))))
                   (:line
                    (funcall recurse)
                    (case context
                      (:code (br))
                      (t     (cxml:text " "))))
                   (:paragraph-break
                    (br))
                   (:section
                    (unwind-protect
                         (progn ; let ((anchor (format nil "section-~A" name)))
                           (assert anchor)
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
                                         ; :function  (a:curry #'visit :related-issues)
                                 )))
                    (when (bp:node-relation builder '(:required-issue . *) node)
                      (h 2 "Required issues")
                      (cxml:with-element "ul"
                        (funcall recurse :relations '(:required-issue)
                                         ; :function  (a:curry #'visit :required-issues)
                                 )))
                    (funcall recurse :relations '(:section))))))
        (bp:walk-nodes builder (a:curry #'visit t) node)))))
