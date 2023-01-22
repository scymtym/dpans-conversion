(cl:in-package #:dpans-conversion.transform)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;; `read-client'

;;; TODO
;;; + parse macro in (lambda lambda-list {\DeclsAndDoc} (block block-name {} *))
;;; + parse macros in comments
;;; + parse macros in symbol names
;;; + Treat everything after out until end of line as none-code
;;; + delimiter matching
;;; + no warnings for double package markers
;;; + parse macro in unreadable objects (example in WITH-OPEN-FILE)

(defclass read-client (eclector.examples.highlight::highlight-client)
  ((%environment    :initarg :environment
                    :reader  environment)
   (%contains-tex?  :initarg :contains-tex?
                    :reader  contains-tex?)
   (%contains-math? :initarg :contains-math?
                    :reader  contains-math?))
  (:default-initargs
   :environment    (a:required-argument :environment)
   :contains-tex?  (a:required-argument :contains-tex?)
   :contains-math? (a:required-argument :contains-math?)))

(defclass tex-macro (eclector.examples.highlight.cst::inner-node
                     ;; TODO should be eclector.examples.highlight.cst::leaf-node
                     )
  ((%name      :initarg  :name
               :reader   eclector.examples.highlight.cst:name)
   #+no (%arguments :initarg  :arguments
               :reader   arguments
               :initform '())))

(defclass verbatim-node (eclector.examples.highlight.cst::inner-node
                         ;; TODO should be eclector.examples.highlight.cst::leaf-node
                         )
  ((%content :initarg :content
             :reader  content)))

(bp:with-builder ('list)
  (let ((environment (dpans-conversion.parser::augment-environment!
                      (make-instance 'env::lexical-environment :parent dpans-conversion.parser::**meta-environment**
                                     )
                      nil)))
    ;; Disable character syntax category "argument" for the
    ;; #, & and % characters.
    (setf (env:lookup :characters 'env::namespace environment)
          (env:lookup :characters 'env::namespace dpans-conversion.parser::**meta-environment**))
    (dolist (character '(#\# #\& #\% #\~ #\$))
      (setf (env:lookup character :characters environment) :normal))
    (parser.packrat:parse `(dpans-conversion.parser::elements ,environment)
                          "SYS$DISK:[PUBLIC.GAMES]CHESS.DB"
                          :grammar 'dpans-conversion.parser::dpans)))

(defun parse-tex (string environment &key contains-math?)
  (multiple-value-bind (success? position value)
      (bp:with-builder ('list)
        (handler-case
            (let ((environment (dpans-conversion.parser::augment-environment!
                                (make-instance 'env::lexical-environment :parent environment ; dpans-conversion.parser::**meta-environment**
                                               )
                                nil)))
              ;; Disable character syntax category "argument" for the
              ;; #, & and % characters.
              (setf (env:lookup :characters 'env::namespace environment)
                    (env:lookup :characters 'env::namespace dpans-conversion.parser::**meta-environment**))
              (dolist (character '(#\# #\& #\% #\~ ; #\$
                                   ))
                (setf (env:lookup character :characters environment) :normal))
              (unless contains-math?
                (setf (env:lookup #\$ :characters environment) :normal))
              (parser.packrat:parse `(dpans-conversion.parser::elements ,environment)
                                    string
                                    :grammar 'dpans-conversion.parser::dpans))
          (error (c) ; TODO do this properly
            (break "~A" c)
            (values nil))))
    (if (and (eq success? t) (= position (length string)))
        value
        (progn
          (break "~A ~A ~A" success? position (length string))
          (values nil)))))

(defun read-maybe-tex (client stream* &key (prefix '()) contains-math?)
  (loop :with stream = (make-concatenated-stream
                        (make-string-input-stream (coerce prefix 'string))
                        stream*)
        :with readtable   = eclector.reader:*readtable*
        :with token       = (make-array 1 :element-type 'character
                                          :adjustable   t
                                          :fill-pointer 0)
        :with brace-level = 0
        :with dollar?     = nil
        :with escape?     = nil
        :for char = (read-char stream nil nil t)
        :for type = (eclector.readtable:syntax-type readtable char)
        :do (flet ((collect ()
                     (vector-push-extend char token))
                   (done ()
                     (unread-char char stream)
                     (loop-finish)))
              (case char
                ((nil)                  ; end of listing
                 (loop-finish))
                ((#\Space #\Tab #\Newline #\,) ; possible end of token
                 (cond (escape?
                        (collect)
                        (setf escape? nil))
                       ((plusp brace-level)
                        (collect #+no t))
                       (dollar?
                        (collect ))
                       ((a:starts-with-subseq "\\OUT" token)
                        (collect #+no t)
                        (loop :for char = (read-char stream nil nil t)
                              :until (or (null char) (char= char #\Newline))
                              :do (vector-push-extend char token))
                        (vector-push-extend #\Newline token)
                        (loop-finish))
                       (t
                        (done))))
                (#\\
                 (collect)
                 (setf escape? (not escape?)))
                (#\{
                 (collect)
                 (if escape?
                     (setf escape? nil)
                     (incf brace-level))
                 #+no (setf argument (make-array 10 :element-type 'character
                                               :adjustable   t
                                               :fill-pointer 0))
                 )
                (#\}
                 ; (push argument arguments)
                 ; (setf argument nil)
                 (collect)
                 (if escape?
                     (setf escape? nil)
                     (decf brace-level)))
                (#\$
                 (collect)
                 (setf dollar? (not dollar?)))
                (t
                 (if (and (eq type :terminating-macro)
                          (zerop brace-level)
                          (not dollar?))
                     (done)
                     (collect))
                 (setf escape? nil))))
        :finally (return
                   (let ((string      (coerce token 'simple-string))
                         (environment (environment client)))
                     (a:if-let ((parsed (parse-tex string environment
                                                   :contains-math? contains-math?)))
                       (make-instance 'tex-macro :name (bp:node ('list :splice)
                                                         (* (:element . *) parsed)))
                       (eclector.reader:interpret-token
                        client stream string '()))))))

(read-maybe-tex (make-instance 'read-client :environment **meta-environment** :contains-tex? t :contains-math? nil)
                (make-string-input-stream "a\\}"))

(read-maybe-tex (make-instance 'read-client :environment **meta-environment** :contains-tex? t  :contains-math? t)
                (make-string-input-stream "$foo$")
                :contains-math? t)

(read-maybe-tex (make-instance 'read-client :environment **meta-environment** :contains-tex? t :contains-math? t)
                (make-string-input-stream "A")
                :contains-math? t)

;;; Customized reading and result construction
;;;
;;; The main customizations are
;;; 1. recognizing characters like \ and { that initiate TeX parsing
;;; 2. perform TeX parsing in certain contexts

(defmethod eclector.parse-result:make-skipped-input-result
    ((client read-client) (stream t) (reason t) (source t))
  (flet ((augment (&key (start-offset 0) (end-offset 0)
                        (children '()))
           (let* ((node   (call-next-method))
                  (start  (+ (eclector.examples.highlight.cst:start node)
                             start-offset))
                  (end    (- (eclector.examples.highlight.cst:end node)
                             end-offset))
                  (string (subseq (eclector.examples.highlight::input client) start end))
                  (parsed (let ((nodes (parse-tex string (environment client)))) ; TODO make a function for this
                            (make-instance 'tex-macro :name (bp:node ('list :splice)
                                                              (* (:element . *) nodes)))))
                  (source (eclector.base:make-source-range client start end))
                  (result (eclector.parse-result:make-expression-result
                           client parsed '() source)))
             (reinitialize-instance node :children (list* result children)))))
   (etypecase reason
     ((cons (eql :line-comment))
      (augment :start-offset (cdr reason)))
     ((eql :block-comment)
      (augment :start-offset 2 :end-offset 2))
     ((eql *read-suppress*)
      (augment))
     ((cons (member :sharpsign-minus :sharpsign-plus))
      (augment :children (list (cdr reason))))
     ((eql :reader-macro) ; #~zerop from an X3J13 issue triggers this
      (call-next-method)))))

(when nil
  (clouseau:inspect
   (architecture.builder-protocol.inspection:as-tree
    (format-code
     'list '(:listing ()) **meta-environment**
     "#-\\param{test1} \\param{expression1} #+\\param{test2} \\param{expression2}"
     :contains-tex? t)
    'list)))

(when nil
  (clouseau:inspect
   (architecture.builder-protocol.inspection:as-tree
    (format-code
     'list '(:listing ()) **meta-environment**
     "#+\\param{test2}  \\param{foo}"
     :contains-tex? t)
    'list)))

(when nil
  (let ((eclector.base:*client* (make-instance 'read-client :current-package (find-package '#:cl-user)
                                                            :environment     dpans-conversion.parser::**meta-environment**
                                                            :contains-tex?   t)))
    (eclector.parse-result:read-from-string eclector.base:*client* "#+\\param{test2} foo")))
#+no (apply #'eclector.examples.highlight::read-stuff
       :package (find-package '#:cl-user)
       :client (make-instance read-client))

(when nil
  (clouseau:inspect
   (architecture.builder-protocol.inspection:as-tree
    (format-code 'list '(:listing ()) **meta-environment** ";;;; foo {bar}" :contains-tex? t)
    'list)))

(defun read-unreadable (stream)
  (loop :for char = (read-char stream)
        :until (char= char #\>)
        :collect char :into content
        :finally (return
                   (let ((content (coerce content 'string)))
                     (make-instance 'verbatim-node :content content)))))

(defun read-comma (stream char)
  (handler-case
      (eclector.reader::comma stream char)
    (eclector.reader:unquote-not-inside-backquote ()
      (make-instance 'verbatim-node :content ","))))

(defun double-quote (stream char)
  (let ((result (make-array 100 :element-type 'character
                                :adjustable t
                                :fill-pointer 0)))
    (loop for char2 = (eclector.base::read-char-or-recoverable-error
                       stream char 'eclector.reader::unterminated-string
                       :delimiter char :report 'eclector.reader::use-partial-string)
          until (eql char2 char)
          when (eql char2 #\\)
            do (setf char2 (eclector.base::read-char-or-recoverable-error
                            stream nil 'eclector.reader::unterminated-single-escape-in-string
                            :escape-char char2 :report 'eclector.reader::use-partial-string))
               (if (eql char2 #\\)
                   (setf char2 (eclector.base::read-char-or-recoverable-error
                                stream nil 'eclector.reader::unterminated-single-escape-in-string
                                :escape-char char2 :report 'eclector.reader::use-partial-string))
                   (vector-push-extend #\\ result))
          when char2
            do (vector-push-extend char2 result)
          finally (return (copy-seq result)))))

(defun read-string (stream char)
  ;; For a string "CONTENT" parse CONTENT as TeX and push a parse
  ;; result with a source range corresponding to CONTENT (that is
  ;; after the opening " and before the closing ").
  (let* ((client     eclector.base:*client*)
         (start      (eclector.base:source-position client stream))
         (result     (double-quote stream char)) ; TODO wrong; must look for \\" instead of \"
         (end        (eclector.base:source-position client stream))
         (string     (subseq (eclector.examples.highlight::input client) start (1- end)))
         (parsed     (let ((nodes (parse-tex string (environment client)))) ; TODO make a function
                       (make-instance 'tex-macro :name (bp:node ('list :splice)
                                                         (* (:element . *) nodes)))))
         (source     (eclector.base:make-source-range
                      client start (1- end)))
         (tex-result (eclector.parse-result:make-expression-result
                      client parsed '() source)))
    (push tex-result (first eclector.parse-result::*stack*))
    result))

;;; This is a copy of Eclector's `sharpsign-backslash' which does not
;;; upcase the character name.
(defun sharpsign-backslash (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (eclector.reader::numeric-parameter-ignored stream 'sharpsign-backslash parameter))
  (let* ((client  eclector.base:*client*)
         (start  (eclector.base:source-position client stream))
         (char1  (eclector.reader::read-char-or-recoverable-error
                  stream nil 'eclector.reader::end-of-input-after-backslash
                  :report 'eclector.reader::use-replacement-character)))
    (when (null char1)                  ; can happen when recovering
      (return-from sharpsign-backslash #\?))
    (eclector.reader::with-token-info (push-char () finalize :lazy t)
      (flet ((handle-char (char escapep)
               (declare (ignore escapep))
               (when (not (null char1))
                 (push-char char1)
                 (setf char1 nil))
               (push-char char))
             (unterminated-single-escape (escape-char)
               (eclector.reader::%recoverable-reader-error
                stream 'eclector.reader::unterminated-single-escape-in-character-name
                :escape-char escape-char :report 'eclector.reader::use-partial-character-name))
             (unterminated-multiple-escape (delimiter)
               (eclector.reader::%recoverable-reader-error
                stream 'eclector.reader::unterminated-multiple-escape-in-character-name
                :delimiter delimiter :report 'eclector.reader::use-partial-character-name))
             (terminate-character ()
               (return-from sharpsign-backslash
                 (cond (*read-suppress* nil)
                       ((not (null char1)) ; no additional characters pushed
                        ;; Treat the \\ in #\\a as TeX. \\ will expand
                        ;; to \ and thus replace the two input
                        ;; characters with one. "#" and "a" will
                        ;; remain untouched. The rendered result will
                        ;; thus be "#\a".
                        (let* ((end     (eclector.base:source-position client stream))
                               (source2 (eclector.base:make-source-range client (- start 2) (- end 1)))
                               (macro   (eclector.reader:find-character client "\\\\")
                                        #+maybe (let ((old (contains-tex? client)))
                                          (reinitialize-instance client :contains-tex? nil)
                                          (unwind-protect
                                               (eclector.reader:find-character client "\\")
                                            (reinitialize-instance client :contains-tex? old)))))
                          (push (eclector.parse-result:make-expression-result client macro '() source2)
                                (first eclector.parse-result::*stack*))
                          char1))
                       ((let* ((end     (eclector.base:source-position client stream))
                               (source  (eclector.base:make-source-range client (- start 3) end))
                               (source2 (eclector.base:make-source-range client (- start 1) end))
                               (macro   (eclector.reader:find-character client (finalize))))
                          (push (eclector.parse-result:make-expression-result client macro '() source2)
                                (first eclector.parse-result::*stack*))
                          (eclector.parse-result:make-expression-result client #\? '() source)))
                       (t
                        (eclector.reader::%recoverable-reader-error
                         stream 'unknown-character-name
                         :name (finalize)
                         :report 'use-replacement-character)
                        #\?)))))
        (eclector.reader::token-state-machine
         stream eclector.reader::*readtable* handle-char nil nil
         unterminated-single-escape unterminated-multiple-escape
         terminate-character)))))

;;; This readtable is used for code that does not contain TeX
;;; fragments. The only difference to the standard readtable is the
;;; presence of a reader macro for #<...>.
(defvar *minimal-readtable*
  (let ((readtable (eclector.readtable:copy-readtable eclector.reader:*readtable*)))
    (eclector.readtable:set-dispatch-macro-character
     readtable #\# #\< (lambda (stream sub-char parameter)
                         (declare (ignore sub-char parameter))
                         (read-unreadable stream)))
    readtable))

;;; This readtable is used for code that may contain TeX fragments. In
;;; addition to #<, the following macro characters are handled
;;; specially:
;;; * \  -> Parse as possible TeX macro call instead of a symbol
;;; * {  -> Parse as possible TeX group instead of a symbol
;;; * $  -> TODO
;;; * ,  -> Is this sill needed? Maybe this only happens in the context \EV A, B, C
;;; * #\ -> Turn "#\\a" into "#\a" and "#\\\alpha" into "#\α"
(defun make-extended-readtable (&key contains-math?)
  (let ((readtable (eclector.readtable:copy-readtable eclector.reader:*readtable*)))
    ;; TODO Describe
    (eclector.readtable:set-macro-character
     readtable #\\ (lambda (stream char)
                     (unread-char char stream)
                     (read-maybe-tex eclector.base:*client* stream
                                     :contains-math? contains-math?)) ; TODO if parsed as a tex macro, this does not make a suitable cst node around the tex macro
     t)
    (eclector.readtable:set-macro-character
     readtable #\{ (lambda (stream char)
                     (unread-char char stream)
                     (read-maybe-tex eclector.base:*client* stream
                                     :contains-math? contains-math?))
     t)
    (when contains-math?
      (eclector.readtable:set-macro-character
       readtable #\$ (lambda (stream char)
                       (unread-char char stream)
                       (read-maybe-tex eclector.base:*client* stream
                                       :contains-math? t))
       t))
    (eclector.readtable:set-macro-character
     readtable #\, 'read-comma)
    ;;
    (eclector.readtable:set-macro-character
     readtable #\" 'read-string)
    ;; TODO
    (eclector.readtable:set-dispatch-macro-character
     readtable #\# #\< (lambda (stream sub-char parameter)
                         (declare (ignore sub-char parameter))
                         (read-unreadable stream)))

    (eclector.readtable:set-dispatch-macro-character
     readtable #\# #\\ (lambda (stream sub-char parameter)
                         (declare (ignore parameter))
                         (let ((sub-sub-char (read-char stream)))
                           (if (char= sub-sub-char #\\)
                               (sharpsign-backslash stream sub-sub-char nil)
                               ;; Not a character literal, for example #\{.
                               (read-maybe-tex
                                eclector.base:*client* stream
                                :prefix         (list #\# sub-char sub-sub-char)
                                :contains-math? contains-math?)))))

    readtable))

(defvar *extended-readtable*
  (make-extended-readtable))

(defvar *extended-readtable-with-math*
  (make-extended-readtable :contains-math? t))

(defmethod eclector.reader:call-as-top-level-read
    ((client                read-client)
     (thunk                 t)
     (input-stream          t)
     (eof-error-p           t)
     (eof-value             t)
     (preserve-whitespace-p t))
  (let ((eclector.reader:*readtable* (cond ((not (contains-tex? client))
                                            *minimal-readtable*)
                                           ((not (contains-math? client))
                                            *extended-readtable*)
                                           (t
                                            *extended-readtable-with-math*))))
    (call-next-method)))

(defmethod eclector.reader:find-character ((client read-client) (name string))
  ;; A character literal #\Return is written as #\\Return in dpANS
  ;; listings to prevent the \Return part from being interpreted as a
  ;; TeX macro.
  (if (contains-tex? client)
      (read-maybe-tex client (make-string-input-stream name))
      (call-next-method client (string-upcase name))))

(defmethod eclector.reader:read-maybe-nothing ((client read-client) ; TODO does nothing?
                                               (input-stream t)
                                               (eof-error-p t)
                                               (eof-value t))
  (call-next-method))

(defmethod eclector.reader:interpret-token
    ((client        read-client)
     (input-stream  t)
     (token         t)
     (escape-ranges t))
  (if (string= token "...")
      (eclector.reader:interpret-symbol-token client input-stream token nil nil)
      (call-next-method)))

(defmethod eclector.reader:check-feature-expression
    ((client read-client) (feature-expression t))
  t)

(defmethod eclector.reader:evaluate-feature-expression
    ((client read-client) (feature-expression t))
  t)

;;; "Render" the syntax tree for the listing.

(defclass highlight-client ()
  ((%input :initarg  :input
           :reader   input)
   (%stack :accessor stack)))

(defmethod shared-initialize :after ((instance   highlight-client)
                                     (slot-names t)
                                     &key (root nil root-supplied-p))
  (when root-supplied-p
    (setf (stack instance) (list (cons root nil)))))

(defun finish-chunk (client)
  (let* ((builder 'list)
         (top     (first (stack client)))
         (content (cdr top)))
    (unless (a:emptyp content)
      (let ((chunk (bp:node (builder :chunk :content content))))
        (bp:relate builder '(:element . *) (car top) chunk))
      (setf (cdr top) nil))))

(defmethod eclector.examples.highlight.render:enter-node
    ((client highlight-client) (node t))
  (finish-chunk client)
  (let* ((builder 'list)
         (classes (eclector.examples.highlight.render::style-class
                   (c2mop:class-prototype
                    (find-class 'eclector.examples.highlight.render::html-client))
                   node))
         (content (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (node    (bp:make-node builder :syntax :classes classes)))
    (push (cons node content) (stack client))))

(defmethod eclector.examples.highlight.render:leave-node
    ((client highlight-client) (node t))
  (finish-chunk client)
  (let* ((builder 'list)
         (node    (car (pop (stack client))))
         (parent  (car (first (stack client)))))
    (bp:relate builder '(:element . *) parent (bp:finish-node builder :syntax node))))

(defmethod eclector.examples.highlight.render:write-character
    ((client    highlight-client)
     (position  t)
     (character t)
     (node      t))
  (let ((top (first (stack client)))) ; TODO ensure-chunk
    (when (null (cdr top))
      (setf (cdr top)
            (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (vector-push-extend character (cdr top))))

(defmethod eclector.examples.highlight.render:enter-node
    ((client highlight-client)
     (node   eclector.examples.highlight.cst:standard-symbol-node))
  (finish-chunk client)
  (let* ((builder 'list)
         (content (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (name    (eclector.examples.highlight.cst:name node)))
    (multiple-value-bind (name namespace)
        (if (a:starts-with #\& name)
            (values (subseq name 1) :lambda-list-keyword)
            (values name            nil))
      (let* ((title (let ((start (eclector.examples.highlight.cst:start node))
                          (end   (eclector.examples.highlight.cst:end node)))
                      (subseq (input client) start end)))
             (node  (apply #'bp:make-node builder :possible-reference
                           :must-resolve? t
                           (when namespace
                             (list :namespace namespace))))
             (node (bp:relate builder '(:target . 1) node
                              (bp:node (builder :chunk :content name))))
             (node (if title
                       (bp:relate builder '(:title . 1) node
                                  (bp:node (builder :chunk :content title)))
                       node)))
        (push (cons node content) (stack client))))) ; TODO finish-node?
  (call-next-method))

(defmethod eclector.examples.highlight.render:leave-node
    ((client highlight-client) (node eclector.examples.highlight.cst:standard-symbol-node))
  (call-next-method)
  (let* ((builder 'list)
         (node    (car (pop (stack client))))
         (parent  (car (first (stack client)))))
    (bp:relate builder '(:element . *) parent (bp:finish-node builder :possible-reference node))))

(defmethod eclector.examples.highlight.render:enter-node
    ((client highlight-client) (node tex-macro))
  (finish-chunk client)
  (push (cons (eclector.examples.highlight.cst:name node) nil) (stack client)))

(defmethod eclector.examples.highlight.render:leave-node
    ((client highlight-client) (node tex-macro))
  (let* ((builder 'list)
         (node    (car (pop (stack client))))
         (parent  (car (first (stack client)))))
    (bp:relate builder '(:element . *) parent (bp:finish-node builder :syntax node))))

;;; Drop any input characters that where parsed as TeX macros since
;;; the expansion of the macros will be part of the output AST
;;; instead.
(defmethod eclector.examples.highlight.render:write-character
    ((client    highlight-client)
     (position  t)
     (character t)
     (node      tex-macro)))

(defmethod eclector.examples.highlight.render:enter-errors ((client highlight-client) (errors t))
  ;; TODO
  ; (break "~A" (map 'list (a:compose #'princ-to-string #'eclector.examples.highlight.cst::message) errors))
  )

(defmethod eclector.examples.highlight.render:leave-errors ((client highlight-client) (errors t)))

#+unused (defun remove-tex-escapes (string)      ; TODO maybe not necessary?
  (loop :with result = string
        :for index = (or (search "\\\\" result)
                         (search "\\#" result))
        :while index
        :do (setf result (concatenate 'string
                                      (subseq result 0 index)
                                      (subseq result (1+ index))))
        :finally (return result)))

(defun format-code (builder root environment code &key contains-tex?
                                                       contains-math?)
  (let* ((code          code ; (remove-tex-escapes code)
                        )
         (package       "COMMON-LISP-USER" ; (find-package '#:cl-user)
                        )
         (read-client   (make-instance 'read-client :input           code
                                                    :current-package package
                                                    :environment     environment
                                                    :contains-tex?   contains-tex?
                                                    :contains-math?  contains-math?))
         (render-client (make-instance 'highlight-client :input code
                                                         :root  root)))
    (eclector.examples.highlight:highlight
     code                          ; :package (find-package #:common-)
     :read-client read-client
     :client      render-client)
    (car (first (stack render-client)))))

;;; Transform

(defclass parse-listings (environment-mixin
                          default-reconstitute-mixin
                          builder-mixin)
  ())

(defmethod transform-node ((transform parse-listings) recurse
                           relation relation-args node (kind (eql :code)) relations
                           &rest initargs &key content contains-tex? contains-math?)
  (tagbody
   :start
     (restart-case
         (let* ((builder     (builder transform))
                (environment (environment transform))
                (node        (apply #'bp:make-node builder :listing
                                    (a:remove-from-plist initargs :content)))
                (stripped    (strip-leading-space content)))
           (format-code builder node environment stripped
                        :contains-tex?  contains-tex?
                        :contains-math? contains-math?)
           (return-from transform-node (bp:finish-node builder :listing node)))
       (retry ()
         :report "Try parsing the listing again"
         (go :start)))))

;;; Utility

(defun strip-leading-space (string)
  (flet ((map-lines (function)
           (loop :with length = (length string)
                 :for start = 0 :then (1+ end)
                 :for end = (position #\Newline string :start start)
                 :while end
                 :do (funcall function start end)
                 :finally (unless (= start length)
                            (funcall function start length)))))
    (let ((prefix-length most-positive-fixnum))
      (map-lines
       (lambda (start end)
         (a:minf prefix-length
                 (a:if-let ((index (position #\Space string :test  #'char/=
                                                            :start start
                                                            :end   end)))
                   (- index start)
                   0))))
      (with-output-to-string (stream)
        (map-lines (lambda (start end)
                     (write-string string stream
                                   :start  (+ start prefix-length)
                                   :end    end)
                     (write-char #\Newline stream)))))))
(strip-leading-space " (char-name #\\\\a)
\\EV NIL
\\OV \"LOWERCASE-a\"
\\OV \"Small-A\"
\\OV \"LA01\"
")
