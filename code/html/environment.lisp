(cl:in-package #:dpans-conversion.html)

(defvar **meta-environment**
  (let ((environment (make-instance 'env::global-environment)))
    (flet ((register-namespace (name namespace-class)
             (setf (env:lookup name 'env:namespace environment)
                   (make-instance namespace-class))))
      (register-namespace :traversal 'env::equal-namespace) ; TODO temp
      (register-namespace :command   'env::equal-namespace)

      (register-namespace :macro    'env::equal-namespace)

      (register-namespace :term     'env::equal-namespace)
      (register-namespace :type     'env::eq-namespace)
      (register-namespace :special  'env::eq-namespace)
      (register-namespace :function 'env::eq-namespace)
      (register-namespace :mac      'env::eq-namespace) ; TODO
      (register-namespace :variable 'env::eq-namespace)
      (register-namespace :constant 'env::eq-namespace))
    environment))
