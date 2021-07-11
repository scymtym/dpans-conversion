(cl:in-package #:dpans-conversion.html)

(defclass transform (transform:builder-mixin
                     transform:environment-mixin
                     transform:file-tracking-mixin
                     output-directory-mixin
                     static-files-mixin)
  (;;
   (%title-prefix      :initarg  :title-prefix
                       :reader   title-prefix
                       :initform nil)
   ;;
   (%sidebar-transform :reader   sidebar-transform
                       :writer   (setf %sidebar-transform))
   ;;
   (%static-files      :initform (let ((base #.(or *compile-file-pathname*
                                                   *load-pathname*)))
                                   (list #+no (load-time-value
                                               (a:read-file-into-string
                                                ))
                                         (cons #1=#P"style.css"     (merge-pathnames #1# base))
                                         (cons #4=#P"syntax.css"    (merge-pathnames #4# base)) ; TODO make a mixin
                                         (cons #2=#P"permalink.css" (merge-pathnames #2# base))
                                         (cons #3=#P"permalink.js"  (merge-pathnames #3# base)))))))

(defmethod shared-initialize :after ((instance   transform)
                                     (slot-names t)
                                     &key (use-sidebar? nil use-sidebar?-supplied?))
  (when use-sidebar?-supplied?
    (setf (%sidebar-transform instance)
          (if use-sidebar?
              (let ((builder          (transform:builder instance))
                    (output-directory (output-directory instance)))
                (make-instance 'navigation-sidebar :builder          builder
                                                   :output-directory output-directory))
              nil))))

(defmethod prepare-transform ((transform transform))
  (call-next-method)
  (a:when-let ((sidebar-transform (sidebar-transform transform)))
    (prepare-transform sidebar-transform)))

(defmethod transform:apply-transform :before ((transform transform) (ast t))
  (prepare-transform transform))

(defmacro define-render ((kind &rest keyword-parameters) &body body)
  (flet ((process-keyword (parameter)
           (if (symbolp parameter)
               `(,parameter (a:required-argument ,(a:make-keyword parameter)))
               parameter)))
    `(defmethod transform:transform-node
         ((transform transform)
          recurse relation relation-args node (kind (eql ,kind)) relations
          &rest initargs
          &key ,@(map 'list #'process-keyword keyword-parameters)
          &allow-other-keys)
       (declare (ignorable initargs))
       ,@body)))

#+no (define-render (:collection)
  #+no (a:when-let ((sidebar-transform (sidebar-transform transform)))
    (transform:apply-transform sidebar-transform node))

  (let ((output-directory (output-directory transform)))
    ;; Render the specification
    #+not-yet (let ((filename (merge-pathnames "chap-0.html" output-directory)))
                (with-html-document (stream filename :use-mathjax t :use-sidebar t)
                  (funcall recurse :relations '((:specification . 1)))))
    ;; Render issues
    (funcall recurse :relations '((:issue . *)))))
