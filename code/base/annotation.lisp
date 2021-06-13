(cl:in-package #:dpans-conversion.base)

(defmethod make-annotation ((source pathname) (range t) (text t)
                            &key (kind :error))
  (let ((source (sloc:make-source source :content (a:read-file-into-string source))))
    (make-annotation source range text :kind kind)))

(defmethod make-annotation ((source sloc:source) (range cons) (text string)
                            &key (kind :error))
  (destructuring-bind (start . end) range
    (let ((location (sloc:make-location source start end)))
      (sloc:make-annotation location text :kind kind))))
