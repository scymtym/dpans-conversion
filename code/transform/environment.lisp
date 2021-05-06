(cl:in-package #:dpans-conversion.transform)

(defvar **meta-environment**
  (let ((environment (make-instance 'env::global-environment)))
    (flet ((register-namespace (name namespace-class)
             (setf (env:lookup name 'env:namespace environment)
                   (make-instance namespace-class))))
      (register-namespace :traversal 'env::equal-namespace)
      (register-namespace :command   'env::equal-namespace))
    environment))
