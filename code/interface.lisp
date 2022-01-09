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
  (let ((candidates (append (directory (merge-pathnames "proposed/*" directory))
                            (directory (merge-pathnames "passed/*" directory)))))
    (remove-if (lambda (filename)
                 (string= (pathname-name filename) "character-proposal"))
               candidates)))

(defun parse-issues (builder issue-files)
  (a:mappend (lambda (file)
               (with-simple-restart (continue "Skip file ~S" file)
                 (let* ((directory (pathname-directory file))
                        (issues    (nth (- (length directory) 2) directory))
                        (index     (position #\- issues :from-end t))
                        (process   (string-upcase (subseq issues 0 index))))
                  (list (parser:parse-issue-file
                         builder file :process process)))))
             issue-files))

;;; Parsing dpANS sources and cleanup issues into a single "document
;;; collection" syntax tree

(defun parse-specification (builder dpans-directory issues-directory
                            &key (root-tex-file  (merge-pathnames
                                                  "chap-0.tex"
                                                  dpans-directory))
                                 (issue-files    (a:mappend #'find-issues
                                                            (a:ensure-list issues-directory)))
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
  (clouseau:inspect
   (architecture.builder-protocol.inspection:as-tree
    (parse-specification 'list
                         "~/code/cl/common-lisp/dpans-conversion/data/dpANS3/"
                         "~/code/cl/common-lisp/dpans/issues/")
    'list)))

;;; Complete conversion

(defvar *cache*)

(defmacro cached (expression)
  `(if (boundp '*cache*)
       (symbol-value '*cache*)
       (setf *cache* ,expression)))

(defun make-drop-1 (builder)
  (make-instance 'transform::drop :builder   builder
                                  :predicate (a:conjoin (transform::kind? :other-command-application)
                                                        (transform::initarg? :name (a:disjoin
                                                                                    (lambda (value)
                                                                                      (a:when-let ((primitive (tex:find-primitive value)))
                                                                                        (intersection '(:environment :io) ; TODO maybe :misc
                                                                                                      (tex:tags primitive))))
                                                                                    (lambda (value)
                                                                                      (member value '("onecolumn" "twocolumn"
                                                                                                      "indextab"
                                                                                                      "firstindextab")
                                                                                              :test #'string=)))))))

(defun make-drop-2 (builder)
  (make-instance 'transform::drop :builder   builder
                                  :predicate (a:disjoin
                                              (a:conjoin (transform::kind? :input)
                                                         (transform::initarg? :name "setup-for-toc"))
                                              (transform::kind? :definition)
                                              (transform::kind? :assignment)
                                              (transform::kind? :font) ; TODO should be gone at this point
                                              (transform::kind? :chardef)
                                              (transform::kind? :mathchardef)
                                              (transform::kind? :newif)
                                              (transform::kind? :newskip)
                                              (transform::kind? :new)
                                              (transform::kind? :counter-definition)
                                              (transform::kind? :setbox)
                                              (transform::kind? :global)
                                              (transform::kind? :catcode)
                                              (transform::kind? :advance)
                                              (transform::kind? :register-read)
                                              (transform::kind? :comment)
                                              (a:conjoin (transform::kind? :other-command-application)
                                                         (transform::initarg? :name (lambda (value)
                                                                                      (a:when-let ((primitive (tex:find-primitive value)))
                                                                                        (intersection '(:font :layout :misc)
                                                                                                      (tex:tags primitive)))))))))

(defun make-drop-3 (builder)
  (make-instance 'transform::drop :builder   builder
                                  :predicate (transform::kind?
                                              (transform::one-of? :input :file))
                                  :action    :replace-with-children))

(defun to-html (input-directory output-directory
                &key (dpans-directory  (merge-pathnames "dpANS3/" input-directory))
                     (issues-directory (directory (merge-pathnames #P"*-issues/" input-directory)))
                     (title-prefix     "Well-specified Common Lisp — ")
                     (annotations      '())
                     (use-mathjax      t)
                     (use-sidebar      t)
                     (inspect?         t))
  (let* ((builder     'list)
         (environment (make-instance 'env:lexical-environment :parent transform::**meta-environment**))
         (reference-environment (make-instance 'env:lexical-environment :parent transform::**reference-meta-environment**))
         ;; Parse
         (tree        (parse-specification builder dpans-directory issues-directory))
         ;; Transform
         (transformed (transform::apply-transforms
                       (list
                        (make-drop-1 builder)
                        (make-instance 'transform::expand-macros :builder           builder
                                                                 :environment       environment
                                                                 :debug-definition? t
                                                                 :debug-expansion   '())
                        (make-instance 'transform::parse-listings :builder     builder
                                                                  :environment environment)
                        (make-instance 'transform::expand-macros :builder           builder
                                                                 :environment       environment
                                                                 :debug-definition? t
                                                                 :debug-expansion   '())

                        (make-drop-2 builder)
                        (make-instance 'transform::lower-display-tables :builder builder)
                        (make-instance 'transform::cleanup-math :builder builder)
                        (make-instance 'transform::cleanup-components :builder builder)
                        (make-instance 'transform::cleanup-issues :builder builder)
                        (make-instance 'transform::attach-labels :builder builder) ; must be after `lower-display-tables'
                        (make-instance 'transform::add-dictionary-sections :builder builder)
                        (make-instance 'transform::split-into-files :builder builder)
                        (make-drop-3 builder) ; must be after `add-dictionary-sections' and `split-into-files'
                        (make-instance 'transform::symbol-index :builder builder)
                        (make-instance 'transform::table-index :builder builder)
                        (make-instance 'transform::issue-index :builder      builder
                                                               :output-file? t)
                        (make-instance 'transform::note-indices :builder builder)
                        (make-instance 'transform::note-output-file :builder builder)
                        (make-instance 'transform::build-references :builder builder :environment reference-environment)
                                        ; (make-instance 'dpans-conversion.transform::verify)
                        )
                       tree))
         ;; Render
         (result      (let ((transform (make-instance 'dpans-conversion.html::transform
                                                      :environment      environment
                                                      :builder          builder
                                                      :output-directory output-directory
                                                      :title-prefix     title-prefix
                                                      :use-sidebar?     use-sidebar
                                                      :annotations      annotations)))
                        #+no (dpans-conversion.html::render-to-file
                              transformed :output environment
                              :use-sidebar      use-sidebar
                              :use-mathjax      use-mathjax

                              :transform        transform
                              :output-directory output-directory)

                        (transform:apply-transform transform transformed))))
    (when inspect?
      (clouseau:inspect
       (list (cons :evaluation-environment environment)
             (cons :reference-environment  reference-environment)
             (cons :raw-document           (vector (architecture.builder-protocol.inspection:as-tree
                                                    tree 'list)
                                                   (architecture.builder-protocol.inspection:as-query
                                                    tree 'list :editor-note)))
             (cons :processed-document     (vector (architecture.builder-protocol.inspection:as-tree
                                                    transformed 'list)
                                                   (architecture.builder-protocol.inspection:as-query
                                                    transformed 'list :component))))))

    result))

(defun inspect-tree (tree &key environment intermediate-tree)
  (flet ((tree-and-query (tree)
           (values
            (architecture.builder-protocol.inspection:as-tree
             tree 'list)
            (architecture.builder-protocol.inspection:as-query
             tree 'list :editor-note))))
    (multiple-value-bind (intermediate-tree intermediate-query)
        (when intermediate-tree (tree-and-query intermediate-tree))
      (multiple-value-bind (final-tree final-query)
          (tree-and-query tree)
        (let ((data (vector environment
                            :intermediate intermediate-tree intermediate-query
                            :final        final-tree        final-query)))
          (clouseau:inspect data :new-process t))))))

(defun to-sexp (input-directory output-file
                &key (dpans-directory  (merge-pathnames "dpANS3/" input-directory))
                     (issues-directory (directory (merge-pathnames #P"*-issues/" input-directory))))
  (let* ((builder     'list)
         (environment (make-instance 'env:lexical-environment :parent transform::**meta-environment**))
         ;; Parse
         (tree        (cached (parse-specification builder dpans-directory issues-directory)))
         ;; Transform
         (result      (transform::apply-transforms
                       (list
                        (make-drop-1 builder)
                        (make-instance 'transform::expand-macros :builder           builder
                                                                 :environment       environment
                                                                 :debug-definition? t
                                                                 :debug-expansion   '())
                        (make-drop-2 builder)
                        (make-instance 'transform::lower-display-tables :builder builder)
                        (make-instance 'transform::cleanup-math :builder builder)
                        (make-instance 'transform::cleanup-components :builder builder)
                        (make-instance 'transform::cleanup-issues :builder builder)
                        (make-instance 'transform::attach-labels :builder builder) ; must be after `lower-display-tables'
                        (make-instance 'transform::add-dictionary-sections :builder builder)
                        (make-instance 'transform::minimize :builder builder)
                        (make-instance 'transform::build-references :builder builder))
                       tree)))

    (a:with-output-to-file (stream output-file :if-exists :supersede)
      (with-standard-io-syntax
        (write result :stream stream :circle t)))

    (inspect-tree result :environment       environment
                         :intermediate-tree tree)

    nil))

(defun make-environment ()
  (make-instance 'env:lexical-environment
                 :parent transform::**meta-environment**))

(defvar *cache2* nil)

(defvar *document-object-tree*)

(defun to-clim (input-directory
                &key (dpans-directory  (merge-pathnames "dpANS3/" input-directory))
                     (issues-directory (directory (merge-pathnames #P"*-issues/" input-directory))))
  (let* ((builder     'list)
         (environment (make-environment))
         ;; Parse
         (tree        (or nil           ; *cache2*
                          (setf *cache2* (parse-specification builder dpans-directory issues-directory))))
         ;; Transform
         (result      (transform::apply-transforms
                       (list
                        (make-drop-1 builder)
                        (make-instance 'transform::expand-macros :builder           builder
                                                                 :environment       environment
                                                                 :debug-definition? t
                                                                 :debug-expansion   '())
                        (make-instance 'transform::parse-listings :builder     builder
                                                                  :environment environment)
                        (make-instance 'transform::expand-macros :builder           builder
                                                                 :environment       environment
                                                                 :debug-definition? t
                                                                 :debug-expansion   '())
                        (make-drop-2 builder)
                        (make-instance 'transform::simplify :builder builder)
                        (make-instance 'transform::lower-display-tables :builder builder)
                        (make-instance 'transform::cleanup-math :builder builder)
                        (make-instance 'transform::cleanup-components :builder builder)
                        (make-instance 'transform::cleanup-issues :builder builder)
                        (make-instance 'transform::cleanup-bnf-rules :builder builder)
                        (make-instance 'transform::attach-labels :builder builder) ; must be after `lower-display-tables'
                        (make-instance 'transform::add-dictionary-sections :builder builder)
                        (make-drop-3 builder) ; must be after `add-dictionary-sections'
                                        ; (make-instance 'transform::symbol-index :builder builder)
                                        ; (make-instance 'transform::table-index :builder builder)
                        (make-instance 'transform::issue-index :builder      builder
                                                               :output-file? nil)
                                        ; (make-instance 'transform::note-indices :builder builder)
                        (make-instance 'transform::drop-bounds :builder builder) ; must be before simplify
                        (make-instance 'transform::simplify :builder builder)
                        (make-instance 'transform::note-parents :builder builder)
                        (make-instance 'transform::build-references :builder builder))
                       tree)))
    (inspect-tree result :intermediate-tree tree :environment environment)
    (setf *document-object-tree* result)
    nil))

(defun do-it (&key (use-mathjax     t)
                   (use-sidebar     nil)
                   (debug-expansion nil)
                   title-prefix
                   (annotations     '()))
  (apply #'to-html #P"~/code/cl/common-lisp/dpans/" #P"/tmp/output/"
         :use-mathjax use-mathjax
         :use-sidebar use-sidebar
         (append (when title-prefix
                   (list :title-prefix title-prefix))
                 (when annotations
                   (list :annotations annotations)))))
