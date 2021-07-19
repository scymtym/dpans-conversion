(cl:in-package #:dpans-conversion.html)

(define-render (:collection)
  (a:when-let ((sidebar-transform (sidebar-transform transform)))
    (transform:apply-transform sidebar-transform node))

  (funcall recurse)
  #+no (if (zerop include-depth)
           (let* ((builder           (transform:builder transform))
                  (title-node        (find-child-of-kind builder :title node))
                  (title             (transform::to-string builder title-node))
                  (sidebar-transform (sidebar-transform transform)))
             (with-html-document (stream "chap-0.html" (output-directory transform)
                                         :title       title
                                         :use-mathjax t ; use-mathjax
                                         :use-sidebar (when sidebar-transform t))
               (when sidebar-transform
                 (transform:apply-transform sidebar-transform node))
               (div "content" recurse)))
           (with-simple-restart (continue "Skip included file ~S" filename)
             (funcall recurse))))

(define-render (:output-file filename title)
  (let* ((builder  (transform:builder transform))
         (root     (bp:node-relation builder '(:element . 1) node))
         (kind     (bp:node-kind builder root))
         (filename (make-pathname :type     (file-type transform)
                                  :defaults filename)
                   #+no (format nil "~@[~A~]~A.html"
                           (when (eq kind :issue)
                             "issues/")
                           filename))
         (title    (format nil "~@[~A~]~A" (title-prefix transform) title))
         (sidebar-transform (sidebar-transform transform)))
    (with-html-document (stream filename (output-directory transform)
                                :title       title
                                :use-mathjax t ; use-mathjax
                                :use-sidebar (when sidebar-transform t))
      (when sidebar-transform
        (transform:apply-transform sidebar-transform node))
      (div "content" (lambda ()
                       (if (eq kind :issue)
                           (funcall recurse)
                           (render-to-file root :file (transform::environment transform)
                                                :transform        transform
                                                :output-directory (output-directory transform))))))))

(define-render (:file filename include-depth)
  (funcall recurse))

(define-render (:title)
  (h* 1 "title" recurse))

(define-render (:sub-title)
  (h* 2 "subtitle" recurse))

(define-render (:chapter anchor)
  (let* ((builder           (transform:builder transform))
         ; (name-node         (bp:node-relation builder '(:name . 1) node))
         ; (name              (transform::to-string builder name-node))
         (number-node       (bp:node-relation builder '(:id . 1) node))
         (number            (transform::to-string builder number-node)))
    (cxml:with-element "section"
      (id-attribute anchor)
      (h* 1 "section-title" (lambda ()
                              (cxml:text (format nil "~A. " number))
                              (funcall recurse :relations '((:name . 1)))))
      (funcall recurse :relations '(:element)))))

(define-render (:section level anchor)
  (let* ((builder   (transform:builder transform))
         (name-node (bp:node-relation builder '(:name . 1) node))
         (name      (transform::evaluate-to-string builder name-node)))
    (flet ((do-it ()
             (cxml:with-element "section"
               (id-attribute anchor)
               (h* (1+ level) "section-title"
                   (a:curry recurse :relations '((:name . 1))))
               (funcall recurse :relations '((:element . *))))))
      (maybe-removable-text
       transform name #'do-it
       :removable '("Figures" "Contents" "Index" "Credits" "Appendix"
                    "Note" "Example")))))
