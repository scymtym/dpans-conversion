(cl:in-package #:dpans-conversion.tex)

(defclass primitive ()
  ((%name      :initarg :name
               :reader  name)
   (%arguments :initarg :arguments
               :reader  arguments)
   (%modes     :initarg :modes
               :reader  modes)
   (%tags      :initarg :tags
               :reader  tags)))

(defmethod print-object ((object primitive) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A{~{~A~^ ~}} ~{~A~^+~}~@[ [~{~A~^ ~}]~]"
            (name object) (arguments object) (modes object) (tags object))))

(defclass math-symbol (primitive)
  ((%character :initarg :character
               :reader  character)))

;;; TeX primitive registry

(defvar **primitives**
  (make-hash-table :test #'equal))

(defun find-primitive (name)
  (gethash name **primitives**))

(defun (setf find-primitive) (new-value name)
  (setf (gethash name **primitives**) new-value))

(defmacro define-primitive (name-and-options (&rest arguments)
                            &rest initargs
                            &key (modes '(:normal))
                                 (tags  '())
                            &allow-other-keys)
  (destructuring-bind (name &key (class 'primitive))
      (a:ensure-list name-and-options)
    (let ((other-initargs (a:remove-from-plist initargs :modes :tags)))
      `(setf (find-primitive ,name)
             (make-instance ',class :name      ,name
                                    :arguments ',arguments
                                    :modes     ',modes
                                    :tags      ',tags
                                    ,@other-initargs)))))
