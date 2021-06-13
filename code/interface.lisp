(cl:in-package #:dpans-conversion)

;;; Parsing dpANS TeX sources

(defun parse-dpans (builder root-tex-file
                    &key (root-directory (uiop:pathname-directory-pathname root-tex-file)))
  (let ((directory   (uiop:pathname-directory-pathname root-tex-file))
        (environment (make-instance 'env:lexical-environment :parent transform::**meta-environment**))
        (relative    (uiop:enough-pathname root-tex-file root-directory)))
    (parser:parse-tex-file builder root-tex-file :filename relative)))

;;; Parsing X3J13 cleanup issues plain text sources

(defun find-issues (directory)
  (let ((candidates (directory (merge-pathnames "passed/*" directory))))
    (remove-if (lambda (filename)
                 (string= (pathname-name filename) "character-proposal"))
               candidates)))

(defun parse-issues (builder issue-files)
  (a:mappend (lambda (file)
               (with-simple-restart (continue "Skip file ~S" file)
                 (list (parser:parse-issue-file builder file))))
             issue-files))

;;; Parsing dpANS sources and cleanup issues into a single "document
;;; collection" syntax tree

(defun parse-specification (builder dpans-directory issues-directory
                            &key (root-tex-file  (merge-pathnames
                                                  "chap-0.tex"
                                                  dpans-directory))
                                 (issue-files    (find-issues issues-directory))
                                 (root-directory (make-pathname
                                                  :name     nil
                                                  :type     nil
                                                  :defaults (merge-pathnames
                                                             (make-pathname :directory '(:relative :up))
                                                             root-tex-file))))
  (let ((specification (parse-dpans builder root-tex-file
                                    :root-directory root-directory))
        (issues        (parse-issues builder issue-files)))
    (bp:node (builder :collection)
      (1 (:specification . 1) specification)
      (* (:issue         . *) issues))))

(when nil
  (:inspect
   (architecture.builder-protocol.visualization:as-tree
    (parse-specification 'list
                         "~/code/cl/common-lisp/dpans-conversion/data/dpANS3/"
                         "~/code/cl/common-lisp/dpans/issues/")
    'list)
   :new-inspector? t))

(:inspect
 (parser:parse-issue-file 'list "~/code/cl/common-lisp/dpans/issues/passed/closed-stream-operations")
 :new-inspector? t)

(when nil
  (:inspect
   (architecture.builder-protocol.visualization::as-tree
    (nth-value
     1 (parse-specification 'list
                            "~/code/cl/common-lisp/dpans/dpANS3/"
                            "~/code/cl/common-lisp/dpans/issues/"))
    'list)
   :new-inspector? t))

(when nil
  (multiple-value-bind (document issues)

      (let ((issues (nth-value
                     1 (parse-specification 'list
                                            "~/code/cl/common-lisp/dpans/dpANS3/"
                                            "~/code/cl/common-lisp/dpans/issues/")))
            (output-directory "/tmp/output/issues/"))

        (ensure-directories-exist output-directory)
        (map nil (lambda (issue)
                   (let* ((filename (getf (bp:node-initargs 'list issue) :filename))
                          (output   (make-pathname :name     (pathname-name filename)
                                                   :type     "html"
                                                   :defaults output-directory)))
                     (dpans-conversion.html::render-issue issue output)))
             issues))))

;;; Complete conversion

(defvar *cache*)

(defmacro cached (expression)
  `(if (boundp '*cache*)
       (symbol-value '*cache*)
       (setf *cache* ,expression)))

(defun to-html (input-directory output-directory
                &key (dpans-directory  (merge-pathnames "dpANS3/" input-directory))
                     (issues-directory (merge-pathnames "issues/" input-directory))
                     (use-mathjax      t)
                     (use-sidebar      t))
  (let* ((builder     'list)
         (environment (make-instance 'env:lexical-environment :parent transform::**meta-environment**))
         ;; Parse
         (tree        (cached (parse-specification builder dpans-directory issues-directory)))
         ;; Transform
         (transformed (transform::apply-transforms
                       (list ;; (make-instance 'dpans-conversion.transform::strip-comments)
                        (make-instance 'dpans-conversion.transform::expand-macros :builder           builder
                                                                                  :environment       environment
                                                                                  :debug-definition? t
                                                                                  :debug-expansion   '())
                        ;; (make-instance 'dpans-conversion.transform::strip-tex-commands)
                                        ; (make-instance 'dpans-conversion.transform::attach-issue-references)
                                        ; (make-instance 'dpans-conversion.transform::build-references :builder 'list)
                                        ; (make-instance 'dpans-conversion.transform::verify)
                        )
                       tree))
         ;; Render
         (result      (let ((transform (make-instance 'dpans-conversion.html::transform :environment      environment
                                                                                        :builder          builder
                                                                                        :output-directory output-directory
                                                                                        :use-sidebar?     use-sidebar))
                            (specification (bp:node-relation builder '(:specification . 1) transformed)))
                        (let* ((filename (getf (bp:node-initargs 'list specification) :filename))
                               (name     (pathname-name filename))
                               (output   (merge-pathnames (make-pathname :name name
                                                                         :type "html")
                                                          output-directory)))
                          (format t "Generating ~A~%" name)
                          (ensure-directories-exist output)
                          (dpans-conversion.html::render-to-file
                           specification output environment
                           :use-sidebar      use-sidebar
                           :use-mathjax      use-mathjax

                           :transform        transform
                           :output-directory output-directory))

                        (transform:apply-transform transform transformed))))

    (:inspect (vector environment
                      :tree
                      (architecture.builder-protocol.visualization::as-tree

                       tree 'list)
                      (architecture.builder-protocol.visualization::as-query
                       tree 'list :editor-note)
                      :transformed
                      (architecture.builder-protocol.visualization::as-tree
                       transformed 'list)
                      (architecture.builder-protocol.visualization::as-query
                       transformed 'list :component))
     :new-inspector? t)

    result))

(defun do-it (&key (use-mathjax     t)
                   (use-sidebar     nil)
                   (debug-expansion nil))
  (to-html #P"~/code/cl/common-lisp/dpans/" #P"/tmp/output/"
           :use-mathjax use-mathjax
           :use-sidebar use-sidebar))
