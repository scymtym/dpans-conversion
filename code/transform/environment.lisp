(cl:in-package #:dpans-conversion.transform)

(defvar **meta-environment**
  (let ((environment (make-instance 'env::global-environment)))
    (flet ((register-namespace (name namespace-class)
             (setf (env:lookup name 'env:namespace environment)
                   (make-instance namespace-class))))
      (register-namespace :traversal 'env::equal-namespace)
      (register-namespace :macro     'env::equal-namespace)
      (register-namespace :math      'env::equal-namespace)
      (register-namespace :variable  'env::equal-namespace)
      (register-namespace :if        'env::equal-namespace)

      (register-namespace :value     'env::equal-namespace)

      (register-namespace :section            'env::equal-namespace) ; TODO separate meta-environment for reference building
      (register-namespace :figure             'env::equal-namespace)

      (register-namespace :symbol             'env::equal-namespace)
      (register-namespace :type               'env::equal-namespace)
      (register-namespace :macro              'env::equal-namespace)
      (register-namespace :function           'env::equal-namespace)
      (register-namespace :special-operator   'env::equal-namespace)
      (register-namespace :variable           'env::equal-namespace)
      (register-namespace :constant           'env::equal-namespace)
      (register-namespace :declaration        'env::equal-namespace)
      (register-namespace :restart            'env::equal-namespace)
      (register-namespace :method-combination 'env::equal-namespace)

      (register-namespace :glossary         'env::equal-namespace)

      (register-namespace :issue            'env::equal-namespace)
      (register-namespace :proposal         'env::equal-namespace))
    environment))

(defvar **reference-meta-environment**
  (let ((environment (make-instance 'env::global-environment)))
    (flet ((register-namespace (name namespace-class)
             (setf (env:lookup name 'env:namespace environment)
                   (make-instance namespace-class))))
      (register-namespace :section            'env::equal-namespace)
      (register-namespace :figure             'env::equal-namespace)

      (register-namespace :symbol             'env::equal-namespace)
      (register-namespace :type               'env::equal-namespace)
      (register-namespace :macro              'env::equal-namespace)
      (register-namespace :function           'env::equal-namespace)
      (register-namespace :special-operator   'env::equal-namespace)
      (register-namespace :variable           'env::equal-namespace)
      (register-namespace :constant           'env::equal-namespace)
      (register-namespace :declaration        'env::equal-namespace)
      (register-namespace :restart            'env::equal-namespace)
      (register-namespace :method-combination 'env::equal-namespace)

      (register-namespace :glossary           'env::equal-namespace)

      (register-namespace :editor-note        'env::equal-namespace)
      (register-namespace :reviewer-note      'env::equal-namespace)

      (register-namespace :issue              'env::equal-namespace)
      (register-namespace :proposal           'env::equal-namespace)

      ; (register-namespace :index/symbol       'env::equal-namespace)
      (register-namespace :lambda-list-keyword 'env::equal-namespace)
      (register-namespace :index/code         'env::equal-namespace)
      (register-namespace :keyword            'env::equal-namespace)
      (register-namespace :index/text         'env::equal-namespace)
      (register-namespace :index/term         'env::equal-namespace)
      (register-namespace :index/example      'env::equal-namespace)
      (register-namespace :package            'env::equal-namespace))
    environment))

(defun namespaces (environment)
  (let ((result '()))
    (env:map-entries (lambda (key value container)
                       (declare (ignore value container))
                       (unless (member key '(:traversal env:namespace))
                         (push key result)))
                     'env:namespace environment)
    result))
