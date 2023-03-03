(cl:in-package #:dpans-conversion.commandline-interface)

(defun main ()
  (let* ((schema        *schema*)
         (configuration (opt:make-configuration schema))
         (defaults      (configuration.options.sources:make-source :defaults))
         (synchronizer  (make-instance 'opt:standard-synchronizer
                                       :target configuration)))
    (configuration.options.sources:initialize defaults schema)
    (configuration.options.sources:process defaults synchronizer)
    (destructuring-bind (&optional input-directory output-directory &rest rest)
        (uiop:command-line-arguments)
      (unless (and input-directory output-directory)
        (format *error-output* "Usage: dpans-converter INPUT-DIRECTORY OUTPUT-DIRECTORY ~
                                [~{--~/opt::print-name/ ARGUMENT~^ ~}]~%"
                (map 'list #'opt:option-name (opt:options schema)))
        (uiop:quit 1))
      (handler-case
          (flet ((set-value (name raw)
                   (let* ((option (opt:find-option name configuration))
                          (value  (opt:raw->value (opt:option-schema-item option) raw)))
                     (setf (opt:option-value option) value))))
            (set-value "input-directory"  input-directory)
            (set-value "output-directory" output-directory)
            (loop :with arguments = rest
                  :for argument = (pop arguments)
                  :while argument
                  :do (let* ((name (subseq argument 2))
                             (raw  (or (pop arguments)
                                       (error "~@<Missing argument to option ~A.~:>"
                                              argument))))
                        (set-value name raw))))
        (error (condition)
          (princ condition *error-output*)
          (uiop:quit 1))))
    (describe configuration)
    (terpri)
    (let ((format                 (opt:value "format"                :configuration configuration))
          (input-directory        (opt:value "input-directory"       :configuration configuration))
          (output-directory       (opt:value "output-directory"      :configuration configuration))
          (title-prefix           (opt:value "title-prefix"          :configuration configuration))
          (issue-annotations?     (opt:value "issue-annotations"     :configuration configuration))
          (reviewer-annotations?  (opt:value "reviewer-annotations"  :configuration configuration))
          (editor-annotations?    (opt:value "editor-annotations"    :configuration configuration))
          (removable-annotations? (opt:value "removable-annotations" :configuration configuration))
          (sidebar?               (opt:value "with-sidebar"          :configuration configuration))
          (inspect?               (opt:value "inspect"               :configuration configuration)))
      (handler-bind ((error (lambda (condition)
                              (princ condition)
                              (terpri)
                              (continue))))
        (ecase format
          (:html (dpans-conversion::to-html input-directory output-directory
                                            :title-prefix title-prefix
                                            :annotations  (append (when issue-annotations?
                                                                    '(:issue))
                                                                  (when reviewer-annotations?
                                                                    '(:reviewer-note))
                                                                  (when editor-annotations?
                                                                    '(:editor-note))
                                                                  (when removable-annotations?
                                                                    '(:removable-text)))
                                            :use-sidebar  sidebar?
                                            :inspect?     inspect?))
          (:sexp (dpans-conversion::to-sexp input-directory output-directory)))))))
