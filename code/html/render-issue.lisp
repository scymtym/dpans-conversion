(cl:in-package #:dpans-conversion.html)

(define-render (:chunk content)
 (cxml:text content))

(defvar *context* t) ; TODO temporary

(define-render (:line)
  (recurse)
  (case *context*
    (:code (br))
    (t     (cxml:text " "))))

(defun issue-reference-title (builder reference-node target-node
                              &key explicit?)
  (let* ((target-initargs    (when target-node
                               (bp:node-initargs builder target-node)))
         (process            (when target-initargs
                               (getf target-initargs :process)))
         (reference-initargs (bp:node-initargs builder reference-node)))
    (destructuring-bind (&key name proposal (explicit? explicit?)
                         &allow-other-keys)
        reference-initargs
      (when explicit?
        (when process
          (cxml:text process)
          (cxml:text " "))
        (cxml:text "Issue ")) ; TODO use proposal when linking to proposal?
      (cxml:text name)
      (when proposal
        (cxml:text ":")
        (cxml:text proposal)))))

(define-render (:proposal name anchor status)
  (cxml:with-element "section"
    (let ((status (format nil "status-~(~A~)" status)))
      (class-attribute (list status "proposal")))
    (cxml:attribute "id" anchor)
    (h* 2 "section-title" (format nil "Proposal~@[ ~A~]" name))
    (funcall recurse)))

(defun special-section-string (builder relation node)
  (a:when-let ((value (bp:node-relation builder `(,relation . 1) node)))
    (transform::evaluate-to-string builder value)))

(define-render (:issue process status)
  (let ((builder (transform:builder transform))
        (level 2))
    (labels ((visit (recurse relation relation-args node kind relations
                     &rest initargs &key name anchor &allow-other-keys)
               (ecase kind
                 ((:dash :reference :unresolved-reference :issue-reference :proposal :enumeration-list :enumeration-item :line :chunk :paragraph-break :listing :syntax :splice :block)
                  (apply #'transform:transform-node transform recurse relation relation-args node kind relations initargs))
                 (:section
                  (unwind-protect
                       (progn
                         (cxml:with-element "section"
                           (cxml:attribute "id" anchor)
                           (class-attribute
                            (if (equal "Status" name)
                                (let ((status (format nil "status-~(~A~)" status)))
                                  (list status "section"))
                                "section"))
                           (h* level "section-title" name)
                           (incf level)
                           (cond ((or (eql (search "Example" name) 0)
                                      (eql (search "Test Case" name) 0))
                                  (cxml:with-element "pre"
                                    (let ((*context* :code))
                                      (funcall recurse))))
                                 (t
                                  (funcall recurse)))))
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
