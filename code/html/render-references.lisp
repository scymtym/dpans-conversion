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

(defun link (transform reference target namespace title-or-continuation)
  (let ((target (if (typep target 'transform::reference) ; TODO temp
                    (transform::target target)
                    target)))
    (let ((class (format nil "~(~A~)-reference" namespace)))
      (if target
          (let ((url (node-url transform reference target)))
            (a* url class title-or-continuation))
          (span (list class "error")
                (lambda ()
                  (cxml:text (string-downcase namespace))
                  (cxml:text ":")
                  (funcall-or-insert-text title-or-continuation)))))))

(define-render (:reference name namespace target) ; TODO move to different file
  (let ((builder (transform:builder transform)))
    (link transform node target namespace
          (lambda ()
            (let ((target (if (typep target 'transform::reference) ; TODO temp
                              (transform::target target)
                              target)))
              (cond ((bp:node-relation builder '(:title . 1) node)
                     (recurse '(:title . 1)))
                    ((and (member namespace '(:section :proposal))
                          target)
                     (let ((initargs (bp:node-initargs builder target)))
                       (a:if-let ((name (getf initargs :name)))
                         (cxml:text name)
                         (bp:walk-nodes
                          builder
                          (a:curry #'transform:transform-node transform)
                          (bp:node-relation builder '(:name . 1) target))
                         ;; (recurse '(:name . 1))
                         )))
                    ((eq namespace :issue)
                     (issue-reference-title builder node target))
                    (t
                     (cxml:text name))))))))
