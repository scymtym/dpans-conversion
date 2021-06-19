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
                       :initform '("hat"))))

(defmethod initialize-instance :after ((instance expand-macros) &key)
  (let ((environment (environment instance)))
    (dpans-conversion.parser::register-builtin-macros environment)
    (setf (env:lookup "hat" :macro environment)
          (lambda (builder environment)
            (declare (ignore environment))
            (list (bp:node (builder :chunk :content "^")))))))

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
   #'call-next-method transform '((:tt . :traversal)) '(t)))

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

;;; Definition

(defmethod transform-node ((transform expand-macros) recurse
                           relation relation-args node (kind (eql :definition)) relations
                           &key name global?)
  (let ((environment (if t ; global?
                         (root-environment transform)
                         (environment transform))))
    (cond ((functionp (env:lookup name :macro environment
                                  :if-does-not-exist nil))
           (when (debug-definition? transform)
             (format t "~V@TNot overwriting diverted~:[~; global~] macro ~S~%"
                     (* 2 (depth transform)) global? name)))
          (t
           (when (debug-definition? transform)
             (format t "~V@TDefining~:[~; global~] macro ~S~%"
                     (* 2 (depth transform)) global? name))
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
    (setf (env:lookup name :macro environment) 0))
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

(defun lookup-macro (name environment)
  (let ((mode (env:lookup :mode :traversal environment
                          :if-does-not-exist :normal)))
    (ecase mode
      (:normal (env:lookup name :macro environment))
      (:math   (or (env:lookup name :math environment
                               :if-does-not-exist nil)
                   (env:lookup name :macro environment))))))

(defun evaluate-arguments (transform builder node)
  (let ((arguments (bp:node-relation builder '(:argument . *) node)))
    (map 'list (a:curry #'apply-transform transform)
         arguments)))

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
            (first-parameter (first (bp:node-relation builder :argument macro)))
            (level           (if first-parameter ; TODO store level when parsing
                                 (getf (bp:node-initargs builder first-parameter) :level)
                                 1)))
       (map 'list (lambda (element)
                    (substitute-arguments builder element level arguments))
            body)))))

(defun node-bounds (builder node)
  (getf (bp:node-initargs builder node) :bounds))

(defmethod peek-node ((transform expand-macros) builder
                      relation relation-args node (kind (eql :other-command-application)))
  (let* ((environment (environment transform))
         (initargs    (bp:node-initargs builder node))
         (name        (getf initargs :name))
         (macro       (handler-bind
                          ((error (lambda (c)
                                    (declare (ignore c))
                                    (let ((stream *standard-output*))
                                      (text.source-location.print:print-annotations
                                       stream (list (dpans-conversion.base:make-annotation
                                                     (env:lookup :current-file :traversal environment)
                                                     (node-bounds builder node) "invoked here" :kind :info)))))))
                        (lookup-macro name environment))))
    (cond
      ((typep macro '(or function (cons keyword)))
       (let* ((arguments (bp:node-relation builder '(:argument . *) node) ; (evaluate-arguments transform builder node)
                         )
              (expansion (expand builder environment macro arguments))
              (debug     (debug-expansion transform)))
         (when (or (eq debug t)
                   (member name debug :test #'string-equal))
           (let ((stream *standard-output*))
             (format stream "~V@T" (* 2 (depth transform)))
             (pprint-logical-block (stream (list node) :per-line-prefix "| ")
               (format stream "Expanded ~A ~:S -> ~S~@:_~@:_" name arguments expansion)
               (text.source-location.print:print-annotations
                stream (list (dpans-conversion.base:make-annotation
                              (env:lookup :current-file :traversal environment)
                              (node-bounds builder macro) "defined here" :kind :info)
                             (dpans-conversion.base:make-annotation
                              (env:lookup :current-file :traversal environment)
                              (node-bounds builder node) "invoked here" :kind :info))))
             (fresh-line stream)))
         (let ((instead (bp:node (builder :splice :expansion-of node)
                          (* (:element . *) expansion))))
           (values instead :splice '() '((:element . *))))))
      (t
       t))))

#+no (cond (*math?*
            (let ((arguments (map 'list (a:curry #'evaluate-to-string builder)
                                  (bp:node-relation builder '(:argument . *) node))))
              (cxml:text (format nil "\\~A~@[{~{~A~}}~] " name arguments)))
            nil)
           ((member name '("newif" "overfullrule" "pageno"
                           "Head" "HeadI" "longbookline"
                           "DocumentNumber" "vfill" "vfil" "hfill" "hfil" "noalign"
                           "eject" "break"
                           "newskip" "newdimen" "hsize" "topskip"
                           "leftskip" "parindent" "parskip"
                           "setbox" "hbox" "fullhsize" "vskip" "hskip" "parfillskip" "relax"
                           "obeylines" "rightskip" "noindent" "hangindent" "negthinspace"
                           "quad" "penalty" "Vskip" "medbreak"
                           "bye")
                    :test 'string=)
            nil)
           (t
            #+no (destructuring-bind (start . end) (getf initargs :source)
                   (cerror "Put error indicator into output"
                           "Undefined macro ~S [~A at ~A:~A in ~A]"
                           name
                           (make-snippet (a:read-file-into-string (first file-stack))
                                         start end)
                           start end (first file-stack)))
            (span "error" (lambda ()
                            (cxml:text (format nil "Undefined macro ~S (source: ~S)"
                                               name (getf initargs :source)))))
            nil))
