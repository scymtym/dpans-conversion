(cl:in-package #:dpans-conversion.html)

;;; `navigation-sidebar'

(defclass navigation-sidebar (transform:builder-mixin
                              output-directory-mixin
                              static-files-mixin)
  ((%static-files :initform (list #+no (load-time-value
                                        (a:read-file-into-string
                                         ))
                                  (cons #1=#P"navigation.js"
                                        (merge-pathnames
                                         #1# #.(or *compile-file-pathname*
                                                   *load-pathname*)))))
   ;; State
   (%index-files  :accessor index-files
                  :initform nil)
   (%current-file :accessor current-file)))

(defmethod transform:transform-node ((transform navigation-sidebar)
                                     recurse relation relation-args node kind relations &key)
  (funcall recurse))

(defmethod transform:transform-node ((transform navigation-sidebar)
                                     recurse relation relation-args node (kind (eql :collection)) relations
                                     &key)
  (let* ((builder (transform:builder transform))
         (files   (bp:node-relation builder '(:element . *) node)))
    (setf (index-files transform)
          (remove-if-not (lambda (file)
                           (member (getf (bp:node-initargs builder file) :output-file)
                                   '("chap-0" "symbol-index" "figure-index" "issue-index")
                                   :test #'equalp))
                         files))
    (break "~A ~A" (length (index-files transform)) (index-files transform) files)))

(defmethod transform:transform-node ((transform navigation-sidebar)
                                     recurse relation relation-args node (kind (eql :output-file)) relations
                                     &key)
  (setf (current-file transform) node)

  (cxml:with-element "nav"
    (cxml:attribute "class" "sidebar")
    (let ((builder (transform:builder transform)))
      (cxml:with-element "ol"
        (map nil (lambda (file)
                   (cxml:with-element "li"
                     (let* ((section (transform::find-ancestor-of-kind builder :section file))
                            (url     (node-url transform node section))
                            (name    (node-name section)))
                       (a url (lambda () (cxml:text name))))))
             (index-files transform))))
    (cxml:with-element "hr")
    (cxml:with-element "ol"
      (class-attribute "local-toc")
      (funcall recurse))))

(defmethod transform:transform-node ((transform navigation-sidebar)
                                     recurse relation relation-args node (kind (eql :section)) relations
                                     &key)
  (cxml:with-element "li"
    (let* ((builder (transform:builder transform))
           (name    (if (bp:node-relation builder '(:name . 1) node)
                        (transform::evaluate-to-string
                         builder (bp:node-relation builder '(:name . 1) node))
                        (getf (bp:node-initargs builder node) :name)))
           (url     (node-url transform (current-file transform) node)))
      (a url (lambda () (cxml:text name))))
    (cxml:with-element "ol"             ; TODO avoid if empty
      (cxml:text " ")
      (funcall recurse)))

  #+no (flet ((emit (target name)
                (cxml:with-element "li"
                  (a target (lambda () (cxml:text name)))
                  (cxml:with-element "ol" ; TODO avoid if empty
                    (cxml:text " ")
                    (funcall recurse)))))

         (let ((builder (transform:builder transform)))
           (if (bp:node-relation builder '(:name . 1) node)
               (let* ((id-node (find-child-of-kind builder :define-section node))
                      (id      (if id-node
                                   (node-name id-node)
                                   (remove-if (a:rcurry #'member '(#\Space #\Newline))
                                              (node-name node))))
                      (target  (format nil "#section-~A" id))
                      (name    (transform::evaluate-to-string
                                builder (bp:node-relation builder '(:name . 1) node))))
                 (emit target name))
               (let* ((name    (getf (bp:node-initargs builder node) :name)) ; TODO different format. this is what issues use
                      (target  (format nil "#section-~A" name)))
                 (emit target name))))))

(defmethod transform:transform-node ((transform navigation-sidebar)
                                     recurse relation relation-args node (kind (eql :component)) relations
                                     &key)
  (cxml:with-element "li"
    (let* ((builder    (transform:builder transform))
           (name-nodes (bp:node-relation builder '(:name . *) node))
           (first-node (first name-nodes))
           (url        (node-url transform (current-file transform) first-node))
           (names      (map 'list (a:curry #'transform::evaluate-to-string builder) ; TODO what about setf?
                            name-nodes))
           (name       (format nil "~{~A~^, ~}" names)))
      (a url (lambda () (cxml:text name))))))
