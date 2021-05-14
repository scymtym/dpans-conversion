(cl:in-package #:dpans-conversion.transform)

(defvar **meta-environment**
  (let ((environment (make-instance 'env::global-environment)))
    (flet ((register-namespace (name namespace-class)
             (setf (env:lookup name 'env:namespace environment)
                   (make-instance namespace-class))))
      (register-namespace :traversal 'env::equal-namespace)
      (register-namespace :command   'env::equal-namespace)

      (register-namespace :section          'env::equal-namespace)

      (register-namespace :symbol           'env::equal-namespace)
      (register-namespace :type             'env::equal-namespace)
      (register-namespace :macro            'env::equal-namespace)
      (register-namespace :function         'env::equal-namespace)
      (register-namespace :special-operator 'env::equal-namespace)
      (register-namespace :variable         'env::equal-namespace)
      (register-namespace :constant         'env::equal-namespace)
      (register-namespace :declaration      'env::equal-namespace)
      (register-namespace :restart          'env::equal-namespace)

      (register-namespace :glossary         'env::equal-namespace))
    environment))
