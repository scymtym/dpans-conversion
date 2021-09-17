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
  ((%environment :initarg :environment
                 :reader  environment)))

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

(defun parse-tex (string environment)
  (multiple-value-bind (success? position value)
      (bp:with-builder ('list)
        (handler-case
            (let ((environment (dpans-conversion.parser::augment-environment!
                                (make-instance 'env::lexical-environment :parent environment)
                                nil)))
              (parser.packrat:parse `(dpans-conversion.parser::elements ,environment)
                                    string
                                    :grammar 'dpans-conversion.parser::dpans))
          (error (c) ; TODO do this properly
            (break "~A" c)
            (values nil))))
    (if (and (eq success? t) (= position (length token)))
        (make-instance 'tex-macro :name (bp:node ('list :splice)
                                          (* (:element . *) value)))
        (progn
          (when (search "IN" string)
            (break "~A ~A ~A" success? position (length token)))
          (eclector.reader:interpret-token
           client stream (coerce token 'simple-string) '())))))

(defun read-maybe-tex (client stream)
  (loop :with readtable   = eclector.reader:*readtable*
        :with token       = (make-array 1 :element-type 'character
                                          :adjustable   t
                                          :fill-pointer 0)
           #+no (:with name        = (make-array 2 :element-type 'character
                                                   :adjustable   t
                                                   :fill-pointer 0))
        :with argument    = nil
        :with arguments   = '()
        :with brace-level = 0
        :for char = (read-char stream nil nil t)
        :for type = (eclector.readtable:syntax-type readtable char)
        :do (flet ((collect (#+no for-tex)
                     (vector-push-extend char token)
                     #+no (when for-tex
                       (vector-push-extend char (or argument name))))
                   (done ()
                     (unread-char char stream)
                     (loop-finish)))
              (case char
                ((nil) ; end of listing
                 (loop-finish))
                ((#\Space #\Tab #\Newline #\,) ; possible end of token
                 (cond ((plusp brace-level)
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
                 (collect #+no nil))
                (#\{
                 (incf brace-level)
                 (setf argument (make-array 10 :element-type 'character
                                               :adjustable   t
                                               :fill-pointer 0))
                 (collect #+no nil))
                (#\}
                 (push argument arguments)
                 (setf argument nil)
                 (decf brace-level)
                 (collect #+no nil))
                (t
                 (if (and (eq type :terminating-macro)
                          (zerop brace-level))
                     (done)
                     (collect #+no t)))))
        :finally (return
                   (let ((string      (coerce token 'simple-string))
                         (environment (environment client)))
                     (parse-tex string environment)))))

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

(defvar *extended-readtable*
  (let ((readtable (eclector.readtable:copy-readtable eclector.reader:*readtable*)))
    (eclector.readtable:set-macro-character
     readtable #\\ (lambda (stream char)
                     (unread-char char stream)
                     (read-maybe-tex eclector.base:*client* stream))
     t)
    (eclector.readtable:set-macro-character
     readtable #\{ (lambda (stream char)
                     (unread-char char stream)
                     (read-maybe-tex eclector.base:*client* stream))
     t)
    (eclector.readtable:set-macro-character
     readtable #\, 'read-comma)
    (eclector.readtable:set-dispatch-macro-character
     readtable #\# #\< (lambda (stream char sub-char)
                         (declare (ignore char sub-char))
                         (read-unreadable stream)))
    readtable))

(defmethod eclector.reader:call-as-top-level-read
    ((client                read-client)
     (thunk                 t)
     (input-stream          t)
     (eof-error-p           t)
     (eof-value             t)
     (preserve-whitespace-p t))
  (let ((eclector.reader:*readtable* *extended-readtable*))
    (call-next-method)))

(defmethod eclector.reader:find-character ((client read-client) (name t))
  (call-next-method))

(defmethod eclector.reader:read-maybe-nothing ((client read-client)
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
  (when (null (cdr (first (stack client))))
    (setf (cdr (first (stack client)))
          (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
  (vector-push-extend character (cdr (first (stack client)))))

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

(defun remove-tex-escapes (string)      ; TODO maybe not necessary?
  (loop :with result = string
        :for index = (or (search "\\\\" result)
                         (search "\\#" result))
        :while index
        :do (setf result (concatenate 'string
                                      (subseq result 0 index)
                                      (subseq result (1+ index))))
        :finally (return result)))

(defun format-code (builder root environment code)
  (let* ((code          code ; (remove-tex-escapes code)
                        )
         (package       "COMMON-LISP-USER" ; (find-package '#:cl-user)
                        )
         (read-client   (make-instance 'read-client :input           code
                                                    :current-package package
                                                    :environment     environment))
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
                           &rest initargs &key content)
  (tagbody
   :start
     (restart-case
         (let* ((builder     (builder transform))
                (environment (environment transform))
                (node        (apply #'bp:make-node builder :listing
                                    (a:remove-from-plist initargs :content))))
           (format-code builder node environment content)
           (return-from transform-node (bp:finish-node builder :listing node)))
       (retry ()
         "Try parsing the listing again"
         (go :start)))))
