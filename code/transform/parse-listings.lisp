(cl:in-package #:dpans-conversion.transform)

;;; `read-client'

;;; TODO
;;; + parse macro in (lambda lambda-list {\DeclsAndDoc} (block block-name {} *))
;;; + parse macros in comments
;;; + Treat everything after out until end of line as none-code
;;; + delimiter matching
;;; + no warnings for double package markers
;;; + parse macro in unreadable objects (example in WITH-OPEN-FILE)

(defclass read-client (eclector.examples.highlight::highlight-client)
  ((%environment   :initarg :environment
                   :reader  environment)
   (%contains-tex? :initarg :contains-tex?
                   :reader  contains-tex?))
  (:default-initargs
   :environment   (a:required-argument :environment)
   :contains-tex? (a:required-argument :contains-tex?)))

(defclass tex-macro (eclector.examples.highlight.cst::inner-node
                     ;; TODO should be eclector.examples.highlight.cst::leaf-node
                     )
  ((%name      :initarg  :name
               :reader   eclector.examples.highlight.cst:name)
   (%arguments :initarg  :arguments
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

(defun parse-tex (string environment)
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
              (dolist (character '(#\# #\& #\% #\~ #\$))
                (setf (env:lookup character :characters environment) :normal))
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

(defun read-maybe-tex (client stream* &key (prefix '()))
  (loop :with stream = (make-concatenated-stream (make-string-input-stream (coerce prefix 'string))
                                                 stream*)
        :with readtable   = eclector.reader:*readtable*
        :with token       = (make-array 1 :element-type 'character
                                          :adjustable   t
                                          :fill-pointer 0)
        :with brace-level = 0
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
                (t
                 (if (and (eq type :terminating-macro)
                          (zerop brace-level))
                     (done)
                     (collect))
                 (setf escape? nil))))
        :finally (return
                   (let ((string      (coerce token 'simple-string))
                         (environment (environment client)))
                     (a:if-let ((parsed (parse-tex string environment)))
                       (make-instance 'tex-macro :name (bp:node ('list :splice)
                                                         (* (:element . *) parsed)))
                       (eclector.reader:interpret-token
                        client stream string '()))))))

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
                  (start  (eclector.examples.highlight.cst:start node))
                  (end    (eclector.examples.highlight.cst:end node))
                  (string (subseq (eclector.examples.highlight::input client)
                                  (+ start start-offset) (- end end-offset)))
                  (parsed (let ((nodes (parse-tex string (environment client)))) ; TODO make a function for this
                            (make-instance 'tex-macro :name (bp:node ('list :splice)
                                                              (* (:element . *) nodes)))))
                  (source (eclector.base:make-source-range
                           client (+ start start-offset) (- end end-offset)))
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

     (t
      (break "~A" reason)
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

(defun read-string (stream char)
  ;; For a string "CONTENT" parse CONTENT as TeX and push a parse
  ;; result with a source range corresponding to CONTENT (that is
  ;; after the opening " and before the closing ").
  (let* ((client     eclector.base:*client*)
         (start      (eclector.base:source-position client stream))
         (result     (eclector.reader::double-quote stream char))
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

(when nil
  (clouseau:inspect
   (architecture.builder-protocol.inspection:as-tree
    (format-code 'list '(:listing ()) **meta-environment** "\" \" baz" :contains-tex? t)
    'list)))

(when nil
  (clouseau:inspect
   (architecture.builder-protocol.inspection:as-tree
    (format-code
     'list '(:listing ()) **meta-environment**
     "\"IN
% baz\""
     :contains-tex? t)
    'list)))


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
                               (macro   (eclector.reader:find-character client "\\\\")))
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
;;; * ,  -> Is this sill needed? Maybe this only happens in the context \EV A, B, C
;;; * #\ -> Turn "#\\a" into "#\a" and "#\\\alpha" into "#\α"
(defvar *extended-readtable*
  (let ((readtable (eclector.readtable:copy-readtable eclector.reader:*readtable*)))
    ;; TODO Describe
    (eclector.readtable:set-macro-character
     readtable #\\ (lambda (stream char)
                     (unread-char char stream)
                     (read-maybe-tex eclector.base:*client* stream)) ; TODO if parsed as a tex macro, this does not make a suitable cst node around the tex macro
     t)
    (eclector.readtable:set-macro-character
     readtable #\{ (lambda (stream char)
                     (unread-char char stream)
                     (read-maybe-tex eclector.base:*client* stream))
     t)
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
                                :prefix (list #\# sub-char sub-sub-char))))))

    readtable))

(defmethod eclector.reader:call-as-top-level-read
    ((client                read-client)
     (thunk                 t)
     (input-stream          t)
     (eof-error-p           t)
     (eof-value             t)
     (preserve-whitespace-p t))
  (let ((eclector.reader:*readtable* (if (contains-tex? client)
                                         *extended-readtable*
                                         *minimal-readtable*)))
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

;; TODO read-time-evaluation

#+test (let ((eclector.base:*client* (make-instance 'read-client :current-package (find-package '#:cl-user)
                                                          :environment     dpans-conversion.parser::**meta-environment**
                                                          :contains-tex?   t)))
         (eclector.reader:read-from-string "(#\\\\\\alfa #\\\\b 'c #\\{1 2 3\\})"))

(when nil
 (let ((eclector.base:*client* (make-instance 'read-client :current-package (find-package '#:cl-user)
                                                           :environment     dpans-conversion.parser::**meta-environment**
                                                           :contains-tex?   t)))
   (eclector.parse-result:read-from-string eclector.base:*client* "(#\\\\b #\\\\\alpha)")))

(when nil
 (clouseau:inspect
  (architecture.builder-protocol.inspection:as-tree
   (format-code 'list '(:listing ()) **meta-environment** "(#\\\\a)" :contains-tex? t)
   'list)))

(when nil
 (clouseau:inspect
  (architecture.builder-protocol.inspection:as-tree
   (format-code 'list '(:listing ()) **meta-environment** "\"{foo}\"" :contains-tex? t)
   'list)))

#+no (clouseau:inspect
 (architecture.builder-protocol.inspection:as-tree
  (format-code 'list '(:listing ()) **meta-environment** " (append \\lbracket\\ x1\\rbracket \\lbracket\\ x2\\rbracket \\lbracket\\ x3\\rbracket ... \\lbracket\\ xn\\rbracket (quote atom))
" :contains-tex? t)
  'list))

;;;

(defclass highlight-client ()
  ((%stack :accessor stack)))

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
         (which   (a:make-keyword (class-name (class-of node))))
         (content (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (name    (eclector.examples.highlight.cst:name node)))
    (multiple-value-bind (name namespace title)
        (if (a:starts-with #\& name)
            (values (subseq name 1) :lambda-list-keyword name)
            (values name            nil                  nil))
      (let* ((node (apply #'bp:make-node builder :possible-reference
                          :name          name
                          :must-resolve? t
                          (append (when namespace
                                    (list :namespace namespace))
                                  (when title
                                    (list :title title)))))
             (node (if title
                       (bp:relate builder '(:title . 1) node
                                  (bp:node (builder :chunk :content title)))
                       node)))
        (push (cons node content) (stack client)))))
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

(defun format-code (builder root environment code &key contains-tex?)
  (let* ((code          code ; (remove-tex-escapes code)
                        )
         (package       "COMMON-LISP-USER" ; (find-package '#:cl-user)
                        )
         (read-client   (make-instance 'read-client :input           code
                                                    :current-package package
                                                    :environment     environment
                                                    :contains-tex?   contains-tex?))
         (render-client (make-instance 'highlight-client :root root)))
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
                           &rest initargs &key content contains-tex?)
  (tagbody
   :start
     (restart-case
         (let* ((builder     (builder transform))
                (environment (environment transform))
                (node        (apply #'bp:make-node builder :listing
                                    (a:remove-from-plist initargs :content))))
           (format-code builder node environment content
                        :contains-tex? contains-tex?)
           (return-from transform-node (bp:finish-node builder :listing node)))
       (retry ()
         "Try parsing the listing again"
         (go :start)))))
