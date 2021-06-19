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
         (filename (make-pathname :type "html" :defaults filename)
                   #+no (format nil "~@[~A~]~A.html"
                           (when (eq kind :issue)
                             "issues/")
                           filename))
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

(define-render (:title)
  (let ((name (transform::to-string (transform:builder transform) node)))
    (h* 1 "title" name)))

(define-render (:sub-title)
  (let ((name (transform::to-string (transform:builder transform) node)))
    (h* 2 "subtitle" name)))

(define-render (:chapter anchor)
  (let* ((builder           (transform:builder transform))
         (sidebar-transform (sidebar-transform transform))
         (name-node         (bp:node-relation builder '(:name . 1) node))
         (name              (transform::to-string builder name-node))
         (number-node       (bp:node-relation builder '(:id . 1) node))
         (number            (transform::to-string builder number-node))
         (id-node           (bp:node-relation builder '(:name3 . 1) node))
         (id                (transform::to-string builder id-node))
         ; (filename          (format nil "chapter-~A.html" number))
         )
    (cxml:with-element "section"
      (cxml:attribute "id" anchor)
      (h 1 name)
      (funcall recurse :relations '(:element)))
    #+no (with-html-document (stream filename (output-directory transform)
                                :title       (format nil "Chapter ~A: ~A"
                                                     number name)
                                :use-mathjax t ;use-mathjax
                                :use-sidebar (when sidebar-transform t))
      (when sidebar-transform
        (transform:apply-transform sidebar-transform node))
      (div "content"
           (lambda ()
             (let ((anchor (format nil "section-~A" id)))
               (cxml:with-element "section"
                 (cxml:attribute "id" anchor)
                 (h 1 name)
                 (funcall recurse :relations '(:element)))))))))

(define-render (:section level anchor)
  (let* ((builder   (transform:builder transform))
         (name-node (bp:node-relation builder '(:name . 1) node))
         (name      (transform::evaluate-to-string builder name-node))
         (id-node   (find-child-of-kind builder :define-section node))
         (id        (if id-node
                        (node-name id-node)
                        (remove-if (a:rcurry #'member '(#\Space #\Newline))
                                   (node-name node)))))
    (assert (eq (bp:node-kind builder name-node) :chunk))
    (flet ((do-it ()
             (cxml:with-element "section"
               (cxml:attribute "id" anchor)
               (h (1+ level) name)
               (funcall recurse :relations '((:element . *))))))
      (if (find-if (a:rcurry #'a:starts-with-subseq name)
                   '("Note" "Example"))
          (removable-text #'do-it)
          (do-it)))))
