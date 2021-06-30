(cl:in-package #:dpans-conversion.transform)

(defclass cleanup-math (default-reconstitute-mixin
                        builder-mixin)
  ())

(defmethod transform-node ((transform cleanup-math) recurse
                           relation relation-args node (kind (eql :math)) relations
                           &rest initargs &key)
  (let* ((builder  (builder transform))
         (elements (bp:node-relation builder '(:element . *) node)))
    ; (break "~A ~A" elements node)
    (cond (elements
           (let ((content (format nil "~{~A~}"
                                  (map 'list (a:curry #'to-string builder) elements))))
             (cond ((string= content "=")
                    (bp:node (builder :chunk :content "=")))
                   ((a:starts-with #\^ content)
                    (bp:node (builder :superscript)
                      (1 (:element . *) (bp:node (builder :chunk :content (subseq content 1))))))
                   ((a:starts-with #\_ content)
                    (bp:node (builder :subscript)
                      (1 (:element . *) (bp:node (builder :chunk :content (subseq content 1))))))
                   (t
                    (call-next-method) #+no (break)))))
          (t
           (call-next-method)))))
