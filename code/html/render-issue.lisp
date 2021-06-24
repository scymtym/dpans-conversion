(cl:in-package #:dpans-conversion.html)

(define-render (:chunk content)
 (cxml:text content))

(defvar *context* t) ; TODO temporary

(define-render (:line)
  (funcall recurse)
  (case *context*
    (:code (br))
    (t     (cxml:text " "))))

(defun issue-link (transform reference target &key explicit?)
  (let* ((builder            (transform:builder transform))
         (target-initargs    (bp:node-initargs builder target))
         (process            (getf target-initargs :process))
         (reference-initargs (bp:node-initargs builder reference)))
    (destructuring-bind (&key name proposal (explicit? explicit?)
                         &allow-other-keys)
        reference-initargs
      (labels ((format-issue ()
                 (when explicit?
                   (when process
                     (cxml:text process)
                     (cxml:text " "))
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

(define-render (:proposal name anchor status)
  (cxml:with-element "section"
    (let ((status (format nil "status-~(~A~)" status)))
      (class-attribute (list status "proposal")))
    (cxml:attribute "id" anchor)
    (h 2 (format nil "Proposal~@[ ~A~]" name))
    (funcall recurse)))

(defun special-section-string (builder relation node)
  (a:when-let ((value (bp:node-relation builder `(,relation . 1) node)))
    (transform::evaluate-to-string builder value)))

(define-render (:issue process)
  (let ((builder (transform:builder transform))
        (level 2))
    (labels ((visit (recurse relation relation-args node kind relations
                     &rest initargs &key name anchor status &allow-other-keys)
               (ecase kind
                 ((:dash :reference :issue-reference :proposal :enumeration-list :enumeration-item :line :chunk :paragraph-break)
                  (apply #'transform:transform-node transform recurse relation relation-args node kind relations initargs))
                 (:possible-reference
                  (break "should not happen")
                  (a name (lambda () (cxml:text name))))
                 (:section
                  (unwind-protect
                       (progn
                         (cxml:with-element "section"
                           (cxml:attribute "id" anchor)
                           (h level name)
                           (incf level)
                           (if (or (eql (search "Example" name) 0)
                                   (eql (search "Test Case" name) 0))
                               (cxml:with-element "pre"
                                 (let ((*context* :code))
                                   (funcall recurse)))
                               (funcall recurse))))
                    (decf level)))
                 (:preamble)
                 (:issue
                  (let ((string   (special-section-string builder :name node))
                        (forum    (special-section-string builder :forum node))
                        (category (special-section-string builder :category node)))
                    (h 1 (format nil "~A Issue ~A [~A] [~A]"
                                 process string forum category)))
                  (when (bp:node-relation builder '(:related-issue . *) node)
                    (h 2 "Related issues")
                    (cxml:with-element "ul"
                      (funcall recurse :relations '(:related-issue))))
                  (when (bp:node-relation builder '(:required-issue . *) node)
                    (h 2 "Required issues")
                    (cxml:with-element "ul"
                      (funcall recurse :relations '(:required-issue))))
                  (funcall recurse :relations '(:section))))))
      (bp:walk-nodes builder #'visit node))))
