(cl:in-package #:dpans-conversion.transform)

(defun to-string (builder ast &rest asts)
  (let ((transform (make-instance 'to-string :builder builder)))
    (apply-transform transform ast)
    (when asts
      (map nil (a:curry #'apply-transform transform) asts))
    (get-output-stream-string (result transform))))

(defclass to-string (builder-mixin)
  ((%result :initarg  :result
            :reader   result
            :initform (make-string-output-stream))))

(defmethod transform-node ((transform to-string) recurse ; TODO define macro
                           relation relation-args node (kind t) relations
                           &key)
  (funcall recurse))

(defmethod transform-node ((transform to-string) recurse ; TODO define macro
                           relation relation-args node (kind (eql :dash)) relations
                           &key which)
  (let ((string (ecase which
                  (:em "---")
                  (:en "--"))))
    (write-string string (result transform))))

(defmethod transform-node ((transform to-string) recurse ; TODO define macro
                           relation relation-args node (kind (eql :chunk)) relations
                           &key content)
  (write-string content (result transform)))

(defmethod transform-node ((transform to-string) recurse ; TODO define macro
                           relation relation-args node (kind (eql :name)) relations
                           &key content)
  (write-string content (result transform)))

(defmethod transform-node ((transform to-string) recurse ; TODO define macro
                           relation relation-args node (kind (eql :symbol)) relations
                           &key name setf?)
  (let ((result (result transform)))
    (when setf?
      (write-string "(setf ") result)
    (write-string name result)
    (when setf?
      (write-string ")" result))))

(defmethod transform-node ((transform to-string) recurse ; TODO define macro
                           relation relation-args node (kind (eql :possible-reference)) relations
                           &key name)
  (write-string name (result transform)))

(defmethod transform-node ((transform to-string) recurse ; TODO define macro
                           relation relation-args node (kind (eql :reference)) relations
                           &key name namespace)
  (if (eq namespace :lambda-list-keyword)
      (funcall recurse :relations '((:element . *)))
      (write-string name (result transform))))
