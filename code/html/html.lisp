(cl:in-package #:dpans-conversion.html)

;;; File output

(defun call-with-html-document (continuation filename &key use-mathjax use-sidebar)
  (a:with-output-to-file (stream filename
                                 :element-type '(unsigned-byte 8)
                                 :if-exists    :supersede)
    (cxml:with-xml-output (cxml:make-octet-stream-sink stream :omit-xml-declaration-p t)
      (cxml:unescaped "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
                                        ; (cxml:unescaped "<!DOCTYPE html>")
      (cxml:with-element "html"
        (cxml:attribute "xmlns" "http://www.w3.org/1999/xhtml")
        (cxml:with-element "head"
          (cxml:with-element "link"
            (cxml:attribute "rel" "stylesheet")
            (cxml:attribute "type" "text/css")
            (cxml:attribute "href" "style.css"))
          (when use-mathjax
            (cxml:with-element "script"
              (cxml:attribute "src" "https://polyfill.io/v3/polyfill.min.js?features=es6")
              (cxml:text " "))
            (cxml:with-element "script"
              (cxml:attribute "id" "MathJax-script")
              (cxml:attribute "src" "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
              (cxml:text " ")))
          (when use-sidebar
            (cxml:with-element "script"
              (cxml:attribute "src" "navigation.js")
              (cxml:text " "))))
        (cxml:with-element "body"
          (cxml:with-element "main"
            (funcall continuation stream)))))))

(defmacro with-html-document ((stream-var filename &rest args
                                                   &key use-mathjax use-sidebar)
                              &body body)
  (declare (ignore use-mathjax use-sidebar))
  `(call-with-html-document
    (lambda (,stream-var) (declare (ignorable ,stream-var)),@body)
    ,filename ,@args))

;;; Element shortcuts

(defun br ()
  (cxml:with-element "br"))

(defun span (class continuation)
  (cxml:with-element "span"
    (cxml:attribute "class" class)
    (funcall continuation)))

(defun span* (class id continuation)
  (cxml:with-element "span"
    (cxml:attribute "class" class)
    (cxml:attribute "id" id)
    (funcall continuation)))

(defun div (class continuation)
  (cxml:with-element "div"
    (cxml:attribute "class" class)
    (funcall continuation)))

(defun div* (class id continuation)
  (cxml:with-element "div"
    (cxml:attribute "class" class)
    (cxml:attribute "id" id)
    (funcall continuation)))

(defun a (url continuation)
  (cxml:with-element "a"
    (cxml:attribute "href" url)
    (funcall continuation)))

(defun a* (url class continuation)
  (cxml:with-element "a"
    (cxml:attribute "href" url)
    (cxml:attribute "class" class)
    (funcall continuation)))
