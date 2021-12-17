(cl:in-package #:dpans-conversion.html)

;;; File output

(defun filename-and-relative-path (filename output-directory)
  (let* ((filename (merge-pathnames filename output-directory))
         (enough   (uiop:enough-pathname filename output-directory))
         (depth    (1- (length (pathname-directory enough))))
         (ups      (if (plusp depth)
                       (make-list depth :initial-element :up)
                       '()))
         (relative (make-pathname :directory `(:relative ,@ups))))
    (values filename relative)))

(defun call-with-html-document (continuation filename output-directory
                                &key title use-mathjax use-sidebar)
  (multiple-value-bind (filename relative)
      (filename-and-relative-path filename output-directory)
    (ensure-directories-exist filename)
    (a:with-output-to-file (stream filename
                                   :element-type '(unsigned-byte 8)
                                   :if-exists    :supersede)
      (cxml:with-xml-output (cxml:make-octet-stream-sink stream ; :omit-xml-declaration-p t
                                                         )
        #+no (cxml:unescaped "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
        (cxml:unescaped "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1 plus MathML 2.0//EN\"
  \"http://www.w3.org/Math/DTD/mathml2/xhtml-math11-f.dtd\">")
        ;; (cxml:unescaped "<!DOCTYPE html>")
        (cxml:with-element "html"
          (cxml:attribute "xmlns" "http://www.w3.org/1999/xhtml")
          (cxml:with-element "head"
            (cxml:with-element "meta"
              (cxml:attribute "charset" "utf-8"))
            (cxml:with-element "link"
              (cxml:attribute "rel" "stylesheet")
              (cxml:attribute "type" "text/css")
              (cxml:attribute "href" (namestring
                                      (merge-pathnames "style.css" relative))))
            (cxml:with-element "script"
              (cxml:attribute "src" (namestring
                                     (merge-pathnames "permalink.js" relative)))
              ; (cxml:text " ")
              )
            (when use-sidebar
              (cxml:with-element "script"
                (cxml:attribute "src" (namestring
                                       (merge-pathnames "navigation.js" relative)))
                ; (cxml:text " ")
                ))
            (when title
              (cxml:with-element "title"
                (cxml:text title))))
          (cxml:with-element "body"
            (cxml:with-element "main"
              (funcall continuation stream))
            (cxml:with-element "footer"
              (cxml:text "Copyright © 2021 Jan Moringen"))))))))

(defmacro with-html-document ((stream-var filename output-directory
                               &rest args &key title use-mathjax use-sidebar)
                              &body body)
  (declare (ignore title use-mathjax use-sidebar))
  `(call-with-html-document
    (lambda (,stream-var) (declare (ignorable ,stream-var)),@body)
    ,filename ,output-directory ,@args))

;;; Element shortcuts

(defun funcall-or-insert-text (string-or-function)
  (if (stringp string-or-function)
      (cxml:text string-or-function)
      (funcall string-or-function)))

(defun class-attribute (class-or-classes)
  (typecase class-or-classes
    (null)
    (cons (cxml:attribute "class" (format nil "~{~A~^ ~}" class-or-classes)))
    (string (cxml:attribute "class" class-or-classes))))

(defun id-attribute (id)
  (when id (cxml:attribute "id" id)))

(defun nbsp ()
  (cxml:unescaped "&nbsp;"))

(defun br ()
  (cxml:with-element "br"))

(defun <> (local-name class id text-or-continuation)
  (cxml:with-element local-name
    (class-attribute class)
    (id-attribute id)
    (funcall-or-insert-text text-or-continuation)))

(defun span (class text-or-continuation)
  (cxml:with-element "span"
    (class-attribute class)
    (funcall-or-insert-text text-or-continuation)))

(defun span* (class id text-or-continuation)
  (cxml:with-element "span"
    (class-attribute class)
    (id-attribute id)
    (funcall-or-insert-text text-or-continuation)))

(defun div* (class id text-or-continuation)
  (cxml:with-element "div"
    (class-attribute class)
    (id-attribute id)
    (funcall-or-insert-text text-or-continuation)))

(defun div (class text-or-continuation)
  (div* class nil text-or-continuation))

(defun p* (class id text-or-continuation)
  (<> "p" class id text-or-continuation))

(defun a* (url class title-or-continuation)
  (cxml:with-element "a"
    (class-attribute class)
    (cxml:attribute "href" url)
    (funcall-or-insert-text title-or-continuation)))

(defun a (url title-or-continuation)
  (a* url nil title-or-continuation))

(defun h* (level class name-or-continuation)
  (cxml:with-element (coerce (format nil "h~D" level) '(and string (not base-string)))
    (class-attribute class)
    (funcall-or-insert-text name-or-continuation)))

(defun h (level name-or-continuation)
  (h* level nil name-or-continuation))
