(cl:in-package #:dpans-conversion.html)

;;; `output-directory-mixin'

(defclass output-directory-mixin ()
  ((%file-type        :initarg  :file-type ; TODO separate mixin
                      :reader   file-type
                      :initform "xhtml")
   (%output-directory :type     (and pathname
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
  (ensure-directories-exist (output-directory transform)) ; TODO progn method combination?
  (when (next-method-p) (call-next-method)))

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

(defun collect-static-files (files pathname-defaults)
  (flet ((file (name)
           (let* ((pathname (pathname name))
                  (merged   (merge-pathnames
                             pathname pathname-defaults)))
             (cons pathname (if nil
                                (a:read-file-into-string merged)
                                merged)))))
    (map 'list #'file files)))
