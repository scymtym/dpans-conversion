(cl:in-package #:dpans-conversion.transform)

(defclass expand-macros (default-reconstitute-mixin
                         environment-mixin
                         file-tracking-mixin)
  ((%debug-definition? :initarg  :debug-definition?
                       :reader   debug-definition?
                       :initform nil)
   (%debug-expansion   :initarg  :debug-expansion
                       :type     (or (eql t) list)
                       :reader   debug-expansion
                       :initform ())))

(defmethod transform-node ((transform expand-macros) recurse
                           relation relation-args node (kind (eql :definition)) relations
                           &key name)
  (when (debug-definition? transform)
    (format t "~V@TDefining command ~S~%" (* 2 (depth transform)) name))
  (setf (env:lookup name :command (environment transform)) node)
  nil)

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
                       (bp:node (builder :word :content "missing macro argument"))
                                        ; (error "Missing macro argument")
                       )))
               (t
                (apply #'reconstitute builder recurse kind relations initargs)))))
    (bp:walk-nodes builder #'visit body)))

(defun expand (builder macro arguments)
  (let* ((body           (bp:node-relation builder '(:body . *) macro))
         (first-parameter (first (bp:node-relation builder :argument macro)))
         (level           (if first-parameter
                              (getf (bp:node-initargs builder first-parameter) :level)
                              1)))
    (map 'list (lambda (element)
                 (substitute-arguments builder element level arguments))
         body)))

(defmethod transform-node ((transform expand-macros) recurse
                           relation relation-args node (kind (eql :other-command-application)) relations
                           &key name &allow-other-keys)
  (let ((builder 'list))
    (let ((arguments (bp:node-relation builder '(:argument . *) node)))
      (a:if-let ((macro (env:lookup name :macro (environment transform)
                                         :if-does-not-exist nil)))
        (let ((debug     (debug-expansion transform))
              (expansion (expand builder macro arguments)))
          (when (or (eq debug t)
                    (member name debug :test #'equal))
            (format t "  Expanded ~A[~S] -> ~S~%" name arguments expansion))
          (bp:node (builder :splice :expansion-of node)
            (* :element expansion)))
        node
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
                    nil))))))
