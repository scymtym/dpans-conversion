(cl:in-package #:dpans-conversion.transform)

(defun to-string (builder ast)
  (let ((transform (make-instance 'to-string :builder builder)))
    (apply-transform transform ast)))

(defclass to-string (builder-mixin)
  ((%result :reader   result
            :initform (make-string-output-stream))))

(defmethod apply-transform ((transform to-string) (ast t))
  (call-next-method)
  (get-output-stream-string (result transform)))

(defmethod transform-node ((transform to-string) recurse ; TODO define macro
                           relation relation-args node (kind t) relations
                           &key content)
  (funcall recurse))

(defmethod transform-node ((transform to-string) recurse ; TODO define macro
                           relation relation-args node (kind (eql :chunk)) relations
                           &key content)
  (write-string content (result transform)))

(defmethod transform-node ((transform to-string) recurse ; TODO define macro
                           relation relation-args node (kind (eql :name)) relations
                           &key name)
  (write-string name (result transform)))

(defmethod transform-node ((transform to-string) recurse ; TODO define macro
                           relation relation-args node (kind (eql :symbol)) relations
                           &key name setf?)
  (let ((result (result transform)))
    (when setf?
      (write-string "(setf ") result)
    (write-string name result)
    (when setf?
      (write-string ")" result))))
