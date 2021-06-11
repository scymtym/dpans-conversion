(cl:in-package #:dpans-conversion)

(defun parse-dpans (builder root-tex-file
                    &key (root-directory (uiop:pathname-directory-pathname root-tex-file)))
  (let ((directory   (uiop:pathname-directory-pathname root-tex-file))
        (environment (make-instance 'env:lexical-environment :parent transform::**meta-environment**)))
    #+no (flet ((process-setup-file (file)
             (let ((filename (merge-pathnames file directory)))
               (parser:parse-tex-file builder filename))))
      (process-setup-file "setup-title.tex")
      (process-setup-file "setup-aux.tex")
      (process-setup-file "setup-document.tex")
      (process-setup-file "setup-terms.tex"))
    (let* ((relative    (uiop:enough-pathname root-tex-file root-directory))
           (tree        (parser:parse-tex-file builder root-tex-file :filename relative)))
      (:inspect
       (architecture.builder-protocol.visualization:as-tree tree 'list)
       :new-inspector? t)
      #+no (dpans-conversion.transform:apply-transform
            (make-instance 'transform::include-files :builder        builder
                                                     :environment    environment
                                                     :root-directory root-directory)
            tree)
      tree)))

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
  (bp:node (builder :collection)
    (1 (:document . *) (parse-dpans builder root-tex-file
                                    :root-directory root-directory))
    (* (:document . *) (parse-issues builder issue-files))))

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

(defun do-it (&key (use-mathjax     t)
                   (use-sidebar     nil)
                   (debug-expansion nil))
  (let ((env (make-instance 'env:lexical-environment :parent transform::**meta-environment**)))
    (flet ((process-file (input output)
             (format t "Processing file ~A~%" input)
             (with-simple-restart (continue "Skip ~A" input)
               (dpans-conversion.html::render-to-file
                (parser::parse-tex-file 'list input) output env
                :use-mathjax        nil
                :modify-environment t))))
      ;; (process-file "data/dpANS3/setup-title.tex"    "/tmp/output/setup-title.html")
      ;; (process-file "data/dpANS3/setup-aux.tex"      "/tmp/output/setup-aux.html")
      ;; (process-file "data/dpANS3/setup-document.tex" "/tmp/output/setup-document.html")
      ;; (process-file "data/dpANS3/setup-terms.tex"    "/tmp/output/setup-terms.html")
      ;; (:inspect env :new-inspector? t)
      (let* ((tree        (parse-dpans 'list #P "~/code/cl/common-lisp/dpans/dpANS3/chap-0.tex"
                                       :root-directory "~/code/cl/common-lisp/dpans/"))
             ; (toc         (build-toc tree))
             (transformed (transform::apply-transforms
                           (list ;; (make-instance 'dpans-conversion.transform::strip-comments)
                            ;; (make-instance 'dpans-conversion.transform::expand-macros :environment env :debug-expansion '("includeDictionary"))
                            ;; (make-instance 'dpans-conversion.transform::strip-tex-commands)
                                        ; (make-instance 'dpans-conversion.transform::attach-issue-references)
                                        ; (make-instance 'dpans-conversion.transform::build-references :builder 'list)
                                        ; (make-instance 'dpans-conversion.transform::verify)
                            )
                           tree)))

        (:inspect (vector env
                          (architecture.builder-protocol.visualization::as-tree
                           tree 'list)
                          (architecture.builder-protocol.visualization::as-query
                           tree 'list :editor-note)
                          (architecture.builder-protocol.visualization::as-query
                           (transform::apply-transforms
                            (list
                                        ; (make-instance 'dpans-conversion.transform::strip-comments)
                             (make-instance 'transform::expand-macros :builder         'list
                                                                      :environment     env
                                                                      :debug-expansion '("includeDictionary"))
                                        ; (make-instance 'dpans-conversion.transform::strip-tex-commands)

                             )
                            tree)
                           'list
                           :editor-note)
                          (architecture.builder-protocol.visualization::as-query
                           transformed 'list :component)
                          ; toc
                          )
         :new-inspector? t)
        (map nil (lambda (file)
                   (let* ((filename (getf (bp:node-initargs 'list file) :filename))
                          (name     (pathname-name filename))
                          (output   (merge-pathnames (make-pathname :name name
                                                                    :type "html")
                                                     "/tmp/output/")))
                     (format t "Generating ~A~%" name)
                     (ensure-directories-exist output)
                     (with-simple-restart (continue "Skip ~A" filename)
                       (dpans-conversion.html::render-to-file
                        file output env
                        :use-sidebar     use-sidebar
                        :use-mathjax     use-mathjax
                        :debug-expansion debug-expansion))))
             (list transformed) ;; files
             )))))
