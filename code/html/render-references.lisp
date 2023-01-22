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
         (filename           (unless (equal reference-file target-file)
                               (merge-pathnames target-file reference-to-root)))
         (anchor             (getf target-initargs :anchor)))
    (format nil "~:[~*~;~:*~A.~A~]#~A"
            filename (file-type transform) anchor)))

(defun link (transform reference target namespace title-or-continuation)
  (let ((target (if (typep target 'transform::reference) ; TODO temp
                    (transform::target target)
                    target)))
    (let ((class (format nil "~(~A~)-reference" namespace))
          (url   (node-url transform reference target)))
      (a* url class title-or-continuation))))

(defun broken-link (recurse namespace)
  (let ((class (format nil "~(~A~)-reference" namespace)))
    (span (list class "error")
          (lambda ()
            (cxml:text (string-downcase namespace))
            (cxml:text ":")
            (funcall recurse '(:target . 1))))))

(define-render (:reference namespace target)
  (link transform node target namespace (a:curry #'recurse '(:title . 1))))

(define-render (:unresolved-reference namespace)
  (broken-link #'recurse namespace))
