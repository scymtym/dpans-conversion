(cl:in-package #:dpans-conversion.parser)

(defun make-snippet (input start end)
  (let ((raw (subseq input (max 0 (- start 20)) (min (length input) (+ end 20)))))
    (substitute #\¶ #\Newline raw)))

(defun parse-tex-file (builder file &key (filename     (uiop:enough-pathname
                                                        file *default-pathname-defaults*))
                                         include-depth)
  (let ((input (a:read-file-into-string file)))
    (bp:with-builder (builder)
      (multiple-value-bind (result position value)
          (parser.packrat:parse `(document ',filename ',include-depth) input
                                :grammar 'dpans)
        (ecase result
          ((t)    value)
          (:fatal (let ((snippet (make-snippet input position position)))
                    (error "At ~A [~A]: ~A" position snippet value))))))))

(defun parse-issue-file (builder file)
  (let ((input    (a:read-file-into-string file))
        (filename (enough-namestring file)))
    (bp:with-builder (builder)
      (multiple-value-bind (result position value)
          (parser.packrat:parse `(issue ,filename) input :grammar 'issues)
        (ecase result
          ((t)    value)
          ((nil)  (let ((snippet (make-snippet input position position)))
                    (error "At ~A [~A]: incomplete parse" position snippet)))
          (:fatal (let ((snippet (make-snippet input position position)))
                    (error "At ~A [~A]: ~A" position snippet value))))))))
