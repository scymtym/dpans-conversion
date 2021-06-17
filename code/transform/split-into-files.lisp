(cl:in-package #:dpans-conversion.transform)

(defclass split-into-files (default-reconstitute-mixin
                            builder-mixin)
  ((%root :accessor root)))

(defmethod apply-transform ((transform split-into-files) (ast t))
  (let* ((builder (builder transform))
         (root    (bp:make-node builder :collection)))
    (setf (root transform) root)
    (call-next-method)
    (bp:finish-node builder :collection root)))

(defmethod push-output-file ((file t) (transform split-into-files))
  (bp:relate (builder transform) :element (root transform) file))

(defmethod transform-node ((transform split-into-files) recurse
                           relation relation-args node (kind (eql :file)) relations
                           &key include-depth)
  (if (zerop include-depth)
      (let* ((builder (builder transform))
             (file    (bp:node (builder :output-file :filename "chap-0")
                        (1 (:element . 1) (call-next-method)))))
        (push-output-file file transform)
        nil)
      (call-next-method)))

(defmethod transform-node ((transform split-into-files) recurse
                           relation relation-args node (kind (eql :chapter)) relations
                           &key)
  (let* ((builder     (builder transform))
         (number-node (bp:node-relation builder '(:id . 1) node))
         (number      (to-string builder number-node))
         (filename    (format nil "chapter-~A" number))
         (file        (bp:node (builder :output-file :filename filename)
                        (1 (:element . 1) node))))
    (push-output-file file transform)
    nil))

(defmethod transform-node ((transform split-into-files) recurse
                           relation relation-args node (kind (eql :issue)) relations
                           &key)
  (let* ((builder  (builder transform))
         (filename (make-pathname :name      (string-downcase (node-name node))
                                  :directory '(:relative "issues")))
         (file     (bp:node (builder :output-file :filename filename)
                     (1 (:element . 1) node))))
    (push-output-file file transform)
    nil))
