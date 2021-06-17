(cl:in-package #:dpans-conversion.html)

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
         (filename           (merge-pathnames target-file reference-to-root))
         (anchor             (getf target-initargs :anchor)))
    (format nil "~A.html#~A" filename anchor)))

(define-render (:reference name namespace target) ; TODO move to different file
  (let ((builder (transform:builder transform))
        (class   (format nil "~(~A~)-reference" namespace)))
    (if target
        (let* ((target-initargs (bp:node-initargs builder target))
               #+no (name            (or (getf target-initargs :name)
                                    (node-name target)))
               (url             (node-url transform node target)))
          (a* url class (lambda () (cxml:text name))))
        (span (list class "error") (lambda () (cxml:text name))))))
