(cl:in-package #:dpans-conversion.html)

(define-render (:table)
  (span "float"
        (lambda ()
          (cxml:with-element "table"
            #+no (let* ((id-node (find-child-of-kind builder :define-figure node))
                        (id      (when id-node
                                   (node-name id-node)))
                        (anchor  (format nil "figure-~A" id)))
                   (when id (break))
                   (cxml:attribute "id" anchor))
            ;; TODO caption and header
            (funcall recurse :relations '((:row . *))))
          (when (member '(:caption . 1) relations :test #'equal)
            (div "caption" (lambda () (funcall recurse :relations '((:caption . 1)))))))))
