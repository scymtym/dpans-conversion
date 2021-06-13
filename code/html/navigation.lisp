(cl:in-package #:dpans-conversion.html)

;;; `output-directory-mixin'

(defclass output-directory-mixin ()
  ((%output-directory :type     (and directory
                                     (satisfies uiop:directory-pathname-p))
                      :reader   output-directory
                      :writer   (setf %output-directory)))
  (:default-initargs
   :output-directory (a:required-argument :output-directory)))

(defmethod shared-initialize :after
    ((instance   output-directory-mixin)
     (slot-names t)
     &key (output-directory nil output-directory-supplied?))
  (when output-directory-supplied?
    (setf (%output-directory instance)
          (uiop:ensure-directory-pathname output-directory))))

(defmethod prepare-transform ((transform output-directory-mixin))
  (when (next-method-p) (call-next-method)) ; TODO progn method combination?
  (ensure-directories-exist (output-directory transform)))

;;; `static-files-mixin'

(defclass static-files-mixin ()
  ((%static-files :reader static-files)))

(defmethod prepare-transform ((transform static-files-mixin))
  (when (next-method-p) (call-next-method))
  (let ((output-directory (output-directory transform)))
    (map nil (lambda (entry)
               (destructuring-bind (filename . content) entry
                 (let ((content (typecase content
                                  (string   content)
                                  (pathname (a:read-file-into-string content))))
                       (filename (merge-pathnames filename output-directory)))
                   (a:write-string-into-file content filename
                                             :if-exists :supersede))))
         (static-files transform))))

;;;

(defclass navigation-sidebar (transform:builder-mixin
                              output-directory-mixin
                              static-files-mixin)
  ((%static-files :initform (list #+no (load-time-value
                                        (a:read-file-into-string
                                         ))
                                  (cons #1=#P"navigation.js"
                                        (merge-pathnames
                                         #1# #.(or *compile-file-pathname*
                                                   *load-pathname*)))))))

(defmethod transform:transform-node ((transform navigation-sidebar)
                                     recurse relation relation-args node kind relations &key)
  (if (eq kind :issue)
      (cxml:with-element "nav"
        (cxml:attribute "class" "sidebar")
        (cxml:text " ")
        (cxml:with-element "ol"
          (funcall recurse)))
      (funcall recurse)))

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
                (flet ((emit (target name)
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
                                            (remove #\Space (node-name node))))
                               (target  (format nil "#section-~A" id))
                               (name    (transform::evaluate-to-string
                                         builder (bp:node-relation builder '(:name . 1) node))))
                          (emit target name))
                        (let* ((name    (getf (bp:node-initargs builder node) :name))
                               (target  (format nil "#section-~A" name)))
                          (emit target name))))))))
  (define :section)
  (define :sub-section)
  (define :sub-sub-section))
