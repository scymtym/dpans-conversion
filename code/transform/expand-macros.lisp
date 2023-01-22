(cl:in-package #:dpans-conversion.transform)

(defclass expand-macros (builder-mixin
                         peeking-mixin
                         default-reconstitute-mixin
                         environment-mixin
                         file-tracking-mixin)
  (;;
   (%debug-definition? :initarg  :debug-definition?
                       :reader   debug-definition?
                       :initform nil)
   (%debug-expansion   :initarg  :debug-expansion
                       :type     (or (eql t) list)
                       :reader   debug-expansion
                       :initform '())))

(defmethod initialize-instance :after ((instance expand-macros) &key)
  (let ((environment (environment instance)))
    (dpans-conversion.parser::register-builtin-macros environment)
    ;; TODO separate pass for the following?
    (setf (env:lookup "hat" :macro environment)
          (lambda (builder environment)
            (declare (ignore environment))
            (list (bp:node (builder :chunk :content "^"))))

          (env:lookup "uppercase" :macro environment)
          (lambda (builder environment argument)
            (declare (ignore environment))
            (let ((string (to-string builder argument)))
              (list (bp:node (builder :chunk :content (string-upcase string))))))

          (env:lookup "dots" :macro environment)
          (lambda (builder environment)
            (declare (ignore environment))
            (list (bp:node (builder :chunk :content "…"))))

          (env:lookup "raise" :macro environment)
          (lambda (builder environment amount box)
            (declare (ignore environment amount))
            (list (bp:node (builder :superscript)
                    (1 (:left  . *) (bp:node (builder :chunk :content "")))
                    (* (:right . *) (bp:node-relation builder '(:element . *) box)))))

          (env:lookup "strut" :macro environment)
          (lambda (builder environment)
            (declare (ignore builder environment))
            nil)

          (env:lookup "lower" :macro environment)
          (lambda (builder environment amount box)
            (declare (ignore environment))
            (list (bp:node (builder :subscript)
                    (1 (:left  . *) (bp:node (builder :chunk :content "")))
                    (* (:right . *) (bp:node-relation builder '(:element . *) box)))))

          (env:lookup "vrule" :macro environment) ; TODO do we have {h,v}rule?
          (lambda (builder environment length)
            (declare (ignore environment))
            (list (bp:node (builder :chunk :content "|"))))

          ;; This is pretty special. The :caption node has to be
          ;; picked up by a later transform. Renderers don't
          ;; understand :caption nodes.
          (env:lookup "caption" :macro environment)
          (lambda (builder environment content)
            (declare (ignore environment))
            (list (bp:node (builder :caption)
                    (1 (:element . *) content))))

          ;; This is too complicated. It stores the content into a box
          ;; and inserts that box later.
          (env:lookup "Vtop" :macro environment)
          (lambda (builder environment content)
            (declare (ignore environment))
            (list content))

          (env:lookup "vadjust" :macro environment)
          (lambda (builder environment content)
            (declare (ignore builder environment))
            (break "~A" content)
            (list content))

          (env:lookup "endgraf" :macro environment)
          (lambda (builder environment)
            (declare (ignore environment))
            (list (bp:node (builder :paragraph-break))))

          (env:lookup "figref" :macro environment)
          (lambda (builder environment label)
            (declare (ignore environment))
            (unless (eq (bp:node-kind builder label) :other-command-application)
              (break "~S" label))
            (let ((label (getf (bp:node-initargs builder label) :name)))
              (list (bp:node (builder :unresolved-reference :namespace :figure)
                      (1 (:target . 1) (bp:node (builder :chunk :content label))))))))))

;; TODO parse \secref as other-command-application and define a macro diversion above?
(defmethod transform-node ((transform expand-macros) recurse
                           relation relation-args node (kind (eql :unresolved-reference)) relations
                           &key namespace)
  (if (eq namespace :section)
      (let* ((builder (builder transform))
             (name*   (bp:node-relation builder '(:target . 1) node))
             (name    (case (bp:node-kind builder name*)
                        (:other-command-application
                         (getf (bp:node-initargs builder name*) :name))
                        (t
                         (to-string builder name*)))))
        (bp:node (builder :unresolved-reference :namespace :section)
          (1 (:target . 1) (bp:node (builder :chunk :content name)))))
      (call-next-method)))

(defmethod root-environment ((transform expand-macros))
  (labels ((root (environment)
             (let ((parent (env:parent environment)))
               (if (eq parent **meta-environment**)
                   environment
                   (root parent)))))
    (root (environment transform))))

;;; Mode

(macrolet ((define (kind)
             `(defmethod transform-node
                  ((transform expand-macros) recurse
                   relation relation-args node (kind (eql ,kind)) relations
                   &key)
                (call-with-environment
                 #'call-next-method transform '((:mode . :traversal)) '(:math)))))
  (define :math)
  (define :math-display))

(defmethod transform-node
    ((transform expand-macros) recurse
     relation relation-args node (kind (eql :typewriter)) relations
     &key)
  (call-with-environment
   #'call-next-method transform '((:tt . :traversal)) '(t))) ; TODO what does this do?

;;; Conditionals

(defmethod transform-node ((transform expand-macros) recurse
                           relation relation-args node (kind (eql :if)) relations
                           &key test)
  (let ((environment (environment transform))
        (builder     (builder transform)))
    (flet ((branch (which)
             (let ((relation (if which :consequent :alternative)))
               (bp:node (builder :splice :expansion-of node)
                 (* (:element . *) (first (funcall recurse :relations `((,relation . *)))))))))
      (cond ((not (string= test "mmode"))
             ;; TODO should exist but \global\footrue and \global\foofalse does not work yet
             (branch (env:lookup test :value environment :if-does-not-exist nil)))
            ((eq :math (env:lookup :mode :traversal environment
                                         :if-does-not-exist :normal))
             (branch t))
            (t
             (branch nil))))))

;;; Groups

(defmethod transform-node ((transform expand-macros) recurse
                           relation relation-args node (kind (eql :block)) relations
                           &key)
  (let* ((old-environment (environment transform))
         (new-environment (env:augmented-environment old-environment '() '())))
    (setf (environment transform) new-environment)
    (unwind-protect
         (call-next-method)
      (setf (environment transform) old-environment))))

;;; Definition

(defmethod transform-node ((transform expand-macros) recurse
                           relation relation-args node (kind (eql :definition)) relations
                           &key name global?)
  (let* ((environment (if global?
                          (root-environment transform)
                          (environment transform)))
         (builder     (builder transform))
         (arity       (length (bp:node-relation builder :argument node))))
    (cond ((functionp (env:lookup name :macro environment
                                  :if-does-not-exist nil))
           (when t ; (debug-definition? transform)
             (format t "~V@TNot overwriting diverted ~:[local~;global~] macro ~A/~D~%"
                     (* 2 (depth transform)) global? name arity)))
          (t
           (when (debug-definition? transform)
             (format t "~V@TDefining ~:[local~;global~] macro ~A/~D~%"
                     (* 2 (depth transform)) global? name arity))
           (setf (env:lookup name :macro environment) node))))
  nil)

(defmethod transform-node ((transform expand-macros) recurse
                           relation relation-args node (kind (eql :font)) relations
                           &key name global?)
  (let* ((environment (root-environment transform))
         (builder     (builder transform))
         (name        (evaluate-to-string
                       builder (bp:node-relation builder '(:name . 1) node))))
    (when (debug-definition? transform)
      (format t "~V@TDefining font ~S~%"
              (* 2 (depth transform)) name))
    (setf (env:lookup name :macro environment)
          (lambda (builder environment)
            (declare (ignore builder environment)) ; TODO we could emit some :change-font node
            '())))
  nil)

(defmethod transform-node ((transform expand-macros) recurse
                           relation relation-args node (kind (eql :newif)) relations
                           &key)
  (let* ((environment (root-environment transform))
         (builder     (builder transform))
         (name        (evaluate-to-string
                       builder (bp:node-relation builder '(:name . 1) node)))
         (stem        (subseq name 2))
         (true        (concatenate 'string stem "true"))
         (false       (concatenate 'string stem "false")))
    (when (debug-definition? transform)
      (format t "~V@TDefining if ~S -> ~S ~S~%"
              (* 2 (depth transform)) name true false))
    (setf (env:lookup stem  :if    environment) t
          (env:lookup true  :macro environment)
          (lambda (builder environment) ; TODO should go to root environment if \global
            (declare (ignore builder))
            (setf (env:lookup stem :value environment) t)
            nil)
          (env:lookup false :macro environment)
          (lambda (builder environment)
            (declare (ignore builder))
            (setf (env:lookup stem :value environment) nil)
            nil)))
  nil)

;;; Expansion

(defmethod peek-node ((transform expand-macros) builder
                      relation relation-args node kind)
  ;; Leave all nodes that are not macro invocation unchanged.
  t)

(defun lookup-macro (name environment
                     &key (if-does-not-exist nil if-does-not-exist-supplied-p))
  (let ((mode (env:lookup :mode :traversal environment
                          :if-does-not-exist :normal)))
    (ecase mode
      (:normal (apply #'env:lookup name :macro environment
                      (when if-does-not-exist-supplied-p
                        (list :if-does-not-exist if-does-not-exist))))
      (:math   (or (env:lookup name :math environment
                                    :if-does-not-exist nil)
                   (apply #'env:lookup name :macro environment
                          (when if-does-not-exist-supplied-p
                            (list :if-does-not-exist if-does-not-exist))))))))

(defun substitute-arguments (builder body macro-level arguments)
  (labels ((visit (recurse relation relation-args node kind relations
                   &rest initargs &key level number &allow-other-keys)
             (declare (ignore relation relation-args))
             (case kind
               (:argument
                (cond ((not (= macro-level level))
                       node)
                      ((<= 1 number (1+ (length arguments)))
                       (nth (1- number) arguments))
                      (t
                       (error "Missing macro argument"))))
               (t
                (apply #'reconstitute builder recurse kind relations initargs)))))
    (bp:walk-nodes builder #'visit body)))

(defun expand (builder environment macro arguments)
  (typecase macro
    (function
     (apply macro builder environment arguments))
    (t
     (let* ((body            (bp:node-relation builder '(:body . *) macro))
            (parameters      (bp:node-relation builder '(:argument . *) macro))
            (arity           (length parameters))
            (first-parameter (first parameters))
            (level           (if first-parameter ; TODO store level when parsing
                                 (getf (bp:node-initargs builder first-parameter) :level)
                                 1)))
       (unless (= arity (length arguments))
         (error "~@<Macro ~A expects ~D argument~:P, but ~D argument~:P
                 ~:*~[have~;has~:;have~] been supplied.~@:>"
                (getf (bp:node-initargs builder macro) :name)
                arity
                (length arguments)))
       (map 'list (lambda (element)
                    (substitute-arguments builder element level arguments))
            body)))))

(defun node-bounds (builder node)
  (getf (bp:node-initargs builder node) :bounds))

(defmethod peek-node ((transform expand-macros) builder
                      relation relation-args node (kind (eql :other-command-application)))
  (let ((environment (environment transform)))
    (handler-bind
        ((error (lambda (c)
                  (declare (ignore c))
                  (let ((stream *standard-output*))
                    (text.source-location.print:print-annotations
                     stream (list (base:make-annotation
                                   (env:lookup :current-file :traversal environment)
                                   (node-bounds builder node) "invoked here" :kind :info)))))))
      (let* ((initargs (bp:node-initargs builder node))
             (name     (getf initargs :name))
             (macro    (lookup-macro name environment)))
        (cond ((typep macro '(or function (cons keyword)))
               (let* ((arguments (bp:node-relation builder '(:argument . *) node))
                      (expansion (expand builder environment macro arguments))
                      (debug     (debug-expansion transform)))
                 (when (or (eq debug t)
                           (member name '("Adverb") #+no debug :test #'string-equal))
                   (let ((arity  (if (functionp macro)
                                     nil
                                     (length (bp:node-relation builder '(:argument . *) macro))))
                         (stream *standard-output*))
                     (format stream "~V@T" (* 2 (depth transform)))
                     (pprint-logical-block (stream (list node) :per-line-prefix "| ")
                       (format stream "Expanded ~A/~D ~:S -> ~S~@:_~@:_"
                               name arity arguments expansion)
                       (text.source-location.print:print-annotations
                        stream (append (a:when-let ((definition-bounds (node-bounds builder macro)))
                                         (list (base:make-annotation
                                                (env:lookup :current-file :traversal environment)
                                                definition-bounds "defined here" :kind :info)))
                                       (a:when-let ((invocation-bounds (node-bounds builder node)))
                                         (list (base:make-annotation
                                                (env:lookup :current-file :traversal environment)
                                                invocation-bounds "invoked here" :kind :info))))))
                     (fresh-line stream)))
                 (let ((instead (bp:node (builder :splice :expansion-of node)
                                  (* (:element . *) expansion))))
                   (values instead :splice '() '((:element . *))))))
              (t
               t))))))
