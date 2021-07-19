(cl:in-package #:dpans-conversion.html)

(define-render (:index anchor)
  (<> "a" nil anchor ""))

(defun node-url (transform reference-node target-node)
  (let* ((output-directory   (output-directory transform))
         (builder            (transform:builder transform))
         (reference-initargs (bp:node-initargs builder reference-node))
         (reference-file     (getf reference-initargs :output-file))
         (reference-to-root  (nth-value 1 (filename-and-relative-path
                                           reference-file output-directory)))
         (target-initargs    (bp:node-initargs builder target-node))
         (target-file        (getf target-initargs :output-file))
         #+no (target-to-root     (nth-value 1 (filename-and-relative-path
                                           target-file output-directory)))
         (filename           (unless (equal reference-file target-file)
                               (merge-pathnames target-file reference-to-root)))
         (anchor             (getf target-initargs :anchor)))
    (format nil "~:[~*~;~:*~A.~A~]#~A"
            filename (file-type transform) anchor)))

(define-render (:reference name namespace target) ; TODO move to different file
  (let ((builder (transform:builder transform))
        (class   (format nil "~(~A~)-reference" namespace)))
    (if target
        (let ((url (node-url transform node target)))
          (a* url class (lambda ()
                          (cond ((bp:node-relation builder '(:title . 1) node)
                                 (funcall recurse :relations '((:title . 1))))
                                ((member namespace '(:section :proposal))
                                 (let ((initargs (bp:node-initargs builder target)))
                                   (a:if-let ((name (getf initargs :name)))
                                     (cxml:text name)
                                     (bp:walk-nodes
                                      builder
                                      (a:curry #'transform:transform-node transform)
                                      (bp:node-relation builder '(:name . 1) target))
                                     ; (funcall recurse :relations '((:name . 1)))
                                     )))
                                (t
                                 (cxml:text name))))))
        (span (list class "error") (lambda ()
                                     (cxml:text (string-downcase namespace))
                                     (cxml:text ":")
                                     (cxml:text name))))))
