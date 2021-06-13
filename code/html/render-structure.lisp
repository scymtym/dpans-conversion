(cl:in-package #:dpans-conversion.html)

(define-render (:file filename include-depth)
  (if (zerop include-depth)
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

(define-render (:chapter)
  (let* ((builder           (transform:builder transform))
         (sidebar-transform (sidebar-transform transform))
         (number            (transform::evaluate-to-string
                             builder (bp:node-relation builder '(:id . 1) node)))
         (filename          (format nil "chapter-~A.html" number)))
    (with-html-document (stream filename (output-directory transform)
                                :title       (format nil "Chapter ~A" number)
                                :use-mathjax t ;use-mathjax
                                :use-sidebar (when sidebar-transform t))
      (when sidebar-transform
        (transform:apply-transform sidebar-transform node))
      (div "content"
           (lambda ()
             (cxml:with-element "h1"
               (let* ((id     (dpans-conversion.transform::evaluate-to-string
                               builder (bp:node-relation builder '(:name3 . 1) node)))
                      (anchor (format nil "section-~A" id)))
                 (cxml:attribute "id" anchor))
               (funcall recurse :relations '((:name . 1))))
             (funcall recurse :relations '(:element)))))))

(define-render (:section level)
  (let* ((builder   (transform:builder transform))
         (name-node (bp:node-relation builder '(:name . 1) node))
         (name      (transform::evaluate-to-string builder name-node))
         (id-node   (find-child-of-kind builder :define-section node))
         (id        (if id-node
                        (node-name id-node)
                        (remove #\Space (node-name node)))))
    (assert (eq (bp:node-kind builder name-node) :word))
    (flet ((do-it ()
             (let ((anchor (format nil "section-~A" id)))
               (cxml:with-element "section"
                 (cxml:attribute "id" anchor)
                 (h (1+ level) name)
                 (funcall recurse :relations '((:element . *)))))))
      (if (find-if (a:rcurry #'a:starts-with-subseq name)
                   '("Note" "Example"))
          (removable-text #'do-it)
          (do-it)))))
