(cl:in-package #:dpans-conversion)

(defun parse-dpans (builder root-tex-file
                    &key (root-directory (uiop:pathname-directory-pathname root-tex-file)))
  (let ((directory   (uiop:pathname-directory-pathname root-tex-file))
        (environment (make-instance 'env:lexical-environment :parent transform::**meta-environment**)))
    (flet ((process-setup-file (file)
             (let ((filename (merge-pathnames file directory)))
               (parser:parse-tex-file builder filename))))
      (process-setup-file "setup-title.tex")
      (process-setup-file "setup-aux.tex")
      (process-setup-file "setup-document.tex")
      (process-setup-file "setup-terms.tex"))
    (let* ((relative    (uiop:enough-pathname root-tex-file root-directory))
           (tree        (parser:parse-tex-file builder root-tex-file :filename relative)))
      (dpans-conversion.transform:apply-transform
       (make-instance 'transform::include-files :builder        builder
                                                :environment    environment
                                                :root-directory root-directory)
       tree))))

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
                                                             (make-pathname :directory '(:up))
                                                             root-tex-file))))
  (bp:node (builder :collection)
    (1 (:document . *) (parse-dpans builder root-tex-file
                                    :root-directory root-directory))
    (* (:document . *) (parse-issues builder issue-files))))

(:inspect
 (architecture.builder-protocol.visualization:as-tree
  (parse-specification 'list
                       "~/code/cl/common-lisp/dpans-conversion/data/dpANS3/"
                       "~/code/cl/common-lisp/dpans/issues/")
  'list)
 :new-inspector? t)

(:inspect
 (parser:parse-issue-file 'list "~/code/cl/common-lisp/dpans/issues/passed/closed-stream-operations")
 :new-inspector? t)

(:inspect
 (architecture.builder-protocol.visualization::as-tree
  (nth-value
   1 (parse-specification 'list
                          "~/code/cl/common-lisp/dpans/dpANS3/"
                          "~/code/cl/common-lisp/dpans/issues/"))
  'list)
 :new-inspector? t)

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
        issues)))
