(cl:in-package #:dpans-conversion.html)

(defun h (level name)
  (cxml:with-element (coerce (format nil "h~D" level) '(and string (not base-string)))
    (cxml:text name)))

(defun special-section-string (builder relation node)
  (a:when-let ((value (bp:node-relation builder `(,relation . 1) node)))
    (dpans-conversion.transform::evaluate-to-string builder value)))

(defun render-issue (tree file)
  (let ((builder 'list)
        (level   2))
    (a:with-output-to-file (stream file :element-type '(unsigned-byte 8)
                                        :if-exists    :supersede)
      (cxml:with-xml-output (cxml:make-octet-stream-sink stream :omit-xml-declaration-p t
                                                         )
        (cxml:unescaped "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
                                        ; (cxml:unescaped "<!DOCTYPE html>")
        (cxml:with-element "html"
          (cxml:attribute "xmlns" "http://www.w3.org/1999/xhtml")
          (cxml:with-element "head"
            (cxml:with-element "title"
              (let ((string (special-section-string builder :name tree)))
                (cxml:text (format nil "Issue ~A" string))))
            (cxml:with-element "link"
              (cxml:attribute "rel" "stylesheet")
              (cxml:attribute "type" "text/css")
              (cxml:attribute "href" "../style.css")))
          (cxml:with-element "body"
            (labels ((visit (context recurse relation relation-args node kind relations
                             &rest initargs &key name content source explicit? &allow-other-keys)
                       (ecase kind
                         (:dash
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
                         ((:section1 :section2)
                          (unwind-protect
                               (progn
                                 (h level name)
                                 (incf level)
                                 (if (or (eql (search "Example" name) 0)
                                         (eql (search "Test Case" name) 0))
                                     (cxml:with-element "pre"
                                       (funcall recurse :function (a:curry #'visit :code)))
                                     (funcall recurse)))
                            (decf level)))
                         (:preamble)
                         (:issue
                          (let ((string   (special-section-string builder :name tree))
                                (forum    (special-section-string builder :forum tree))
                                (category (special-section-string builder :category tree)))
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
              (bp:walk-nodes builder (a:curry #'visit t) tree))))))))
