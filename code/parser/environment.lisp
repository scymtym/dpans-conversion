(cl:in-package #:dpans-conversion.parser)

(defvar **meta-environment**
  (let ((environment (make-instance 'env::global-environment)))
    (flet ((register-namespace (name namespace-class)
             (setf (env:lookup name 'env:namespace environment)
                   (make-instance namespace-class))))
      (register-namespace :traversal  'env::eq-namespace)
      ;; Associates characters with their currently assigned syntax
      ;; category.
      (register-namespace :characters 'env::eql-namespace)

      (register-namespace :if         'env::equal-namespace)
      (register-namespace :variable   'env::equal-namespace)
      (register-namespace :macro      'env::equal-namespace) ; TODO better have a single entry with slots for normal mode and math mode
      (register-namespace :math       'env::equal-namespace))
    environment))
