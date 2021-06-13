(cl:in-package #:dpans-conversion.html)

(defclass navigation-sidebar (transform::builder-mixin)
  ())

(defmethod transform:transform-node ((transform navigation-sidebar)
                                     recurse relation relation-args node kind relations &key)
  (funcall recurse))

(defmethod transform:transform-node ((transform navigation-sidebar)
                                     recurse relation relation-args node (kind (eql :file)) relations
                                     &key include-depth)
  (if (= include-depth 0)
      (cxml:with-element "nav"
        (cxml:attribute "class" "sidebar")
        (cxml:text " ")
        (funcall recurse))
      (funcall recurse)))

(defmethod transform:transform-node ((transform navigation-sidebar)
                                     recurse relation relation-args node (kind (eql :chapter)) relations &key)
  (cxml:with-element "nav"
    (cxml:attribute "class" "sidebar")
    (cxml:with-element "h2"
      (cxml:text "This chapter"))
    (cxml:with-element "ol"             ; TODO avoid if empty
      (cxml:text " ")
      (funcall recurse))))

(macrolet ((define (kind)
             `(defmethod transform:transform-node ((transform navigation-sidebar)
                                                   recurse relation relation-args node (kind (eql ,kind)) relations &key)
                (let* ((builder (transform::builder transform))
                       (id-node (find-child-of-kind builder :define-section node))
                       (id      (if id-node
                                    (node-name id-node)
                                    (remove #\Space (node-name node))))
                       (target  (format nil "#section-~A" id))
                       (name    (transform::evaluate-to-string
                                 builder (bp:node-relation builder '(:name . 1) node))))
                  (cxml:with-element "li"
                    (a target (lambda () (cxml:text name)))
                    (cxml:with-element "ol" ; TODO avoid if empty
                      (cxml:text " ")
                      (funcall recurse)))))))
  (define :section)
  (define :sub-section)
  (define :sub-sub-section))
