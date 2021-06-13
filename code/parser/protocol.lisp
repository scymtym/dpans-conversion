(cl:in-package #:dpans-conversion.parser)

(define-condition parse-error (base:annotation-mixin)
  ((%message :initarg :message
             :type    string
             :reader  message)) ; TODO format-control
  (:default-initargs
   :message (a:required-argument :message))
  (:report (lambda (condititon stream)
             (a:if-let ((message (message condition)))
               (write-string message stream)
               (format stream "~@<Incomplete parse.~@:>")))))

(defun %parse-tex (builder input environment filename include-depth)
  (bp:with-builder (builder)
    (multiple-value-bind (result position value)
        (parser.packrat:parse `(document ',filename ',include-depth ,environment)
                              input
                              :grammar 'dpans)
      (ecase result
        ((t)    value)
        ((nil)  (error 'parse-error :annotations (list (base:make-annotation filename (cons position (1+ position)) "here"))))
        (:fatal (error 'parse-error :annotations (list (base:make-annotation filename (cons position (1+ position)) "here"))
                                    :message     value))))))

(defun parse-tex-file (builder file &key (filename     (uiop:enough-pathname
                                                        file *default-pathname-defaults*))
                                         include-depth)
  (let ((input       (a:read-file-into-string file))
        (environment (make-instance 'env::lexical-environment :parent **meta-environment**)))

    (setf (env:lookup :global?       :traversal environment) t
          (env:lookup :current-file  :traversal environment) filename
          (env:lookup :include-depth :traversal environment) 0)
    (register-builtin-macros environment)

    (%parse-tex builder input environment filename include-depth)
    #+no (bp:with-builder (builder)
      (multiple-value-bind (result position value)
          (parser.packrat:parse `(document ',filename ',include-depth ,environment)
                                input
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
          ((nil)  (error 'parse-error :annotations (list (base:make-annotation file (cons position (1+ position)) "here"))))
          (:fatal (error 'parse-error :annotations (list (base:make-annotation file (cons position (1+ position)) "here"))
                                      :message     value)))))))
