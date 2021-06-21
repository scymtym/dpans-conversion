(cl:in-package #:dpans-conversion.html)

;;;

(defun format-code (code)
  (with-output-to-string (stream)
    (eclector.examples.highlight:highlight
     code                          ; :package (find-package #:common-)
     :client  (eclector.examples.highlight::make-minimal-client
               :stream stream))))

(defun find-child-of-kind (builder kind node)
  (let ((children (bp:node-relation builder :element node)))
    (find-if (lambda (child)
               (eq (bp:node-kind builder child) kind))
             children)))

(defun tooltip (container-class tooltip-class
                tooltip-continuation content-continuation
                &key (element 'div))
  (funcall element container-class
           (lambda ()
             (span tooltip-class tooltip-continuation)
             (funcall content-continuation))))

(defun removable-text (continuation)
  (tooltip "removable-text" "removable-text-tooltip"
           (lambda ()
             (cxml:text "This passage is ")
             (a "RemovableText"
                (lambda () (cxml:text "removable text")))
             (cxml:text " which is not formerly part of the standard."))
           continuation))

(defun node-name (node)
  (let* ((builder 'list)
         (name    (bp:node-relation builder '(:name . 1) node)))
    (dpans-conversion.transform::evaluate-to-string builder name)))

(defun expand (builder body macro-level arguments)
  (labels ((visit (recurse relation relation-args node kind relations
                   &rest initargs &key level number &allow-other-keys)
             (case kind
               (:argument
                (cond ((not (= macro-level level))
                       (apply #'bp:make+finish-node builder :argument
                              (list* :level (1- level) (a:remove-from-plist initargs :level))))
                      ((<= 1 number (1+ (length arguments)))
                       (nth (1- number) arguments))
                      (t
                       (bp:node (builder :chunk :content "missing macro argument"))
                                        ; (error "Missing macro argument")
                       )))
               (t
                (bp:make+finish-node+relations
                 builder kind initargs
                 (map 'list (lambda (relation)
                              (typecase relation
                                ((or symbol (cons symbol atom))
                                 (multiple-value-bind (relation* cardinality)
                                     (bp:normalize-relation relation)
                                   (declare (ignore relation*))
                                   (let ((right (first (funcall recurse :relations (list relation)))))
                                     (list cardinality relation right))))
                                ((cons t (cons t (cons t null)))
                                 relation)))
                      relations)
                 #+no (map 'list (lambda (relation)
                              (multiple-value-bind (relation* cardinality)
                                  (bp:normalize-relation relation)
                                (ecase cardinality
                                  (1 (list '1 relation (first (funcall recurse :relations (list relation)))))
                                  (* (list '* relation (first (funcall recurse :relations (list relation))))))))
                      relations))))))
    (bp:walk-nodes builder #'visit body)))

(defvar *math?* nil) ; TODO hack

(defun render-name (name setf?)
  (span "name" (lambda ()
                 (when setf?
                   (cxml:text "(setf")
                   (cxml:unescaped "&nbsp;"))
                 (cxml:text name)
                 (when setf?
                   (cxml:text ")")))))

(defun render-name-node (builder name)
  (multiple-value-bind (name setf?)
      (dpans-conversion.transform::evaluate-to-string builder name)
    (render-name name setf?)))

(defun render-node-names (builder node)
  )

(defun builtin-the (environment arguments)
  (break "wtf")
  (assert (a:length= 1 arguments))
  (let* ((argument (first arguments))
         (name     (getf (bp:node-initargs 'list argument) :name)
                   #+no (dpans-conversion.transform::evaluate-to-string
                         'list (first arguments))))
    (assert (eq :other-command-application (bp:node-kind 'list argument)))
    (break "~A ~A => ~A => ~A" environment arguments name (env:lookup name :macro environment
                                                                           :if-does-not-exist nil))
    (a:when-let ((macro (env:lookup name :macro environment :if-does-not-exist nil)))
      (list (bp:node-relation 'list '(:body . *) macro))
      '())))

(defun render-to-file (tree file environment &key (use-mathjax        t)
                                                  (use-sidebar        t)
                                                  (debug-expansion    nil)
                                                  (modify-environment t)
                                                  transform
                                                  output-directory)

  (setf (env:lookup "the" :macro environment) (lambda (&rest args) (apply 'builtin-the args)))

  (let ((builder           'list)
        (stack             '())
        (file-stack        '())
        (environment-stack (list environment)))
    (labels ((push-file (filename)
               (format t "~V@TEmitting for ~A~%"
                       (* 2 (length file-stack))
                       (pathname-name filename))
               (push filename file-stack)
               (unless modify-environment
                 (push (env:augmented-environment (first environment-stack) '() '())
                       environment-stack)))
             (pop-file ()
               (unless modify-environment
                 (pop environment-stack))
               (pop file-stack))
             (peek (builder relation relation-args node)
               (cond ((not (eq (bp:node-kind builder node) :other-command-application))
                      t)
                     (t
                      nil)
                     #+no (t
                           (let* ((initargs  (bp:node-initargs builder node))
                                  (name      (getf initargs :name))
                                  (arguments (bp:node-relation builder '(:argument . *) node)))
                             (a:if-let ((macro (env:lookup name :macro (first environment-stack)
                                                                :if-does-not-exist nil)))
                               (if (functionp macro)
                                   (let ((expansion (funcall macro environment arguments)))
                                     (values (bp:node (builder :splice :expansion-of node)
                                               (* :element expansion))
                                             :splice '() '(:element)))
                                   (let* ((body      (bp:node-relation builder '(:body . *) macro))
                                          (expansion (cond ((equal name "more")
                                                            arguments)
                                                           ((and (= (length body) 1)
                                                                 (eq (bp:node-kind builder (first body)) :other-command-application)
                                                                 arguments)

                                                            (let ((name (getf (bp:node-initargs builder (first body)) :name)))
                                                              (list (bp:node (builder :other-command-application :name name)
                                                                      (* (:argument . *) arguments)))))
                                                           ((not (member name '("sub")
                                                                         :test #'equal))
                                                            (let* ((first-parameter (first (bp:node-relation builder :argument macro)))
                                                                   (level           (if first-parameter
                                                                                        (getf (bp:node-initargs builder first-parameter) :level)
                                                                                        1)))
                                                              (map 'list (lambda (b)
                                                                           #+no (when (equal name "seesection")
                                                                                  (break "~S ~S ~S ~S => ~S" macro level b arguments (expand builder b level arguments)))
                                                                           (expand builder b level arguments))
                                                                   body)))
                                                           (t
                                                            (append body
                                                                    (list (bp:node (builder :block)
                                                                            (* (:element . *) arguments)))
                                        ; (list (bp:node (builder :chunk :content "{")))
                                        ; (bp:node-relation builder :argument node)
                                        ; (list (bp:node (builder :chunk :content "}")))
                                                                    )))))
                                     (when (or (eq debug-expansion t)
                                               (member name debug-expansion :test #'string=))
                                       (format t "  Expanded ~A[~S] -> ~S~%" name arguments expansion))
                                     (values (bp:node (builder :splice :expansion-of node)
                                               (* :element expansion))
                                             :splice '() '(:element))))
                               (cond (*math?*
                                      (let ((arguments (map 'list (a:curry #'evaluate-to-string builder)
                                                            (bp:node-relation builder '(:argument . *) node))))
                                        (cxml:text (format nil "\\~A~@[{~{~A~}}~] " name arguments)))
                                      nil)
                                     ((and (member name '("vtop" "hbox") :test #'string=)
                                           arguments)
                                      (values (bp:node (builder :splice :expansion-of node)
                                                (* (:element . *) arguments))
                                              :splice '() '(:element)))
                                     ((member name '("newif" "overfullrule" "pageno"
                                                     "Head" "HeadI" "longbookline"
                                                     "DocumentNumber" "vfill" "vfil" "hfill" "hfil" "noalign"
                                                     "eject" "vtop"
                                                     "newskip" "newdimen" "hsize" "topskip"
                                                     "leftskip" "parindent" "parskip"
                                                     "setbox" "hbox" "fullhsize" "vskip" "hskip" "parfillskip" "relax"
                                                     "obeylines" "rightskip" "noindent" "hangindent" "negthinspace"
                                                     "quad" "penalty" "Vskip"
                                                     "break" "smallbreak" "medbreak" "goodbreak"
                                                     "bye")
                                              :test 'string=)
                                      nil)
                                     (t
                                      #+no (destructuring-bind (&optional start . end) (getf initargs :source)
                                             (cerror "Put error indicator into output"
                                                     "Undefined macro ~S [~A at ~A:~A in ~A]"
                                                     name
                                                     (when (and start end)
                                                       (dpans-conversion.parser::make-snippet (a:read-file-into-string (first file-stack))
                                                                                              start end))
                                                     start end (first file-stack)))
                                      (span "error" (lambda ()
                                                      (cxml:text (format nil "Undefined macro ~S (source: ~S)"
                                                                         name (getf initargs :source)))))
                                      nil)))))))
             (visit
                 (context recurse relation relation-args node kind relations
                  &rest initargs
                  &key source
                       name
                       global
                       content
                       filename
                       include-depth
                       which
                       editor reviewer
                       anchor
                  &allow-other-keys)
               (unwind-protect
                    (progn
                      (when (> (length stack) 200)
                        (break))
                      (push (if (eq kind :splice)
                                (getf (bp:node-initargs builder (getf (bp:node-initargs builder node) :expansion-of)) :name)
                                kind)
                            stack)
                      (ecase kind
                        #+no (:file
                         (push-file filename)
                         (unwind-protect
                              (if (zerop include-depth)
                                  (with-html-document (stream file output-directory
                                                              :use-mathjax use-mathjax
                                                              :use-sidebar use-sidebar)
                                    (when use-sidebar
                                      (transform:apply-transform
                                       (make-instance 'navigation-sidebar :builder builder :output-directory "/tmp/output/") node))
                                    (div "content" recurse))
                                  (with-simple-restart (continue "Skip included file ~S" filename)
                                    (funcall recurse)))
                           (pop-file)))
                        ;; Evaluation
                        (:definition
                         (break "should not happen")
                         (let* ((kind        (getf initargs :kind))
                                (environment (if global ; TODO make local when result of macro expansion?
                                                 (a:lastcar environment-stack)
                                                 (first environment-stack))))
                           (format t "~V@TDefining ~:[local~;global~] macro ~S~%"
                                   (* 2 (length file-stack)) nil name)
                           (setf (env:lookup name :macro environment) node)))
                        (:splice
                         (funcall recurse))
                        (:argument
                         (span "error" (lambda () (cxml:text (format nil "unresolved macro argument ~S" node)))))
                        ;; Typographic Markup
                        (:chunk
                         (cond ((not *math?*)
                                (cxml:text content))
                               ((equal content "{")
                                (cxml:text "\\{"))
                               ((equal content "}")
                                (cxml:text "\\}"))
                               (t
                                (cxml:text content))))
                        (:bold (span "explicit-bold" recurse))
                        (:italic (span "explicit-italic" recurse))
                        ((:f :typewriter) (span "explicit-mono" recurse))
                        (:roman (span "explicit-roman" recurse))
                        (:hrule
                         (cxml:with-element "hr" (cxml:text " "))) ; HACK


                        ;; Semantic Markup
                        (:term
                         (let* ((name (node-name node))
                                (url  (format nil "#term-~A" name)))
                           (a* url "term" recurse)))
                        (:newterm (span "newterm" recurse))
                        (:newtermidx (span "newterm" (a:curry recurse :relations '((:name . 1)))))

                        (:secref
                         (break "should not happen")
                         (let* ((name (node-name node))
                                (url  (format nil "#section-~A" name)))
                           (a* url "section-reference" recurse)))

                        (:keyref
                         ;; TODO (break "should not happen")
                         (let* ((name (node-name node))
                                (url  (format nil "#lambda-list-keyword-~A" name)))
                           (unless name (break "~A" node))
                           (a* url "lambda-list-keyword-reference" (lambda ()
                                                                     (cxml:text "&")
                                                                     (funcall recurse)))))
                        (:typeref
                         (break "should not happen")
                         (let* ((name (node-name node))
                                (url  (format nil "#type-~A" name)))
                           (unless name (break "~A" node))
                           (a* url "type-reference" recurse)))
                        (:declref
                         (break "should not happen")
                         (let* ((name (node-name node))
                                (url  (format nil "#declaration-~A" name)))
                           (a* url "declaration-reference" recurse)))
                        (:specref
                         (break "should not happen")
                         (let* ((name (node-name node))
                                (url  (format nil "#special-operator-~A" name)))
                           (a* url "special-operator-reference" recurse)))
                        (:funref
                         (break "should not happen")
                         (let* ((name (node-name node))
                                (url  (format nil "#function-~A" name)))
                           (a* url "function-reference" recurse)))
                        (:macref
                         (break "should not happen")
                         (let* ((name (node-name node))
                                (url  (format nil "#macro-~A" name)))
                           (a* url "macro-reference" recurse)))
                        (:varref
                         (break "should not happen")
                         (let* ((name (node-name node))
                                (url  (format nil "#variable-~A" name)))
                           (a* url "variable-reference" recurse)))
                        (:conref
                         (break "should not happen")
                         (let* ((name (node-name node))
                                (url  (format nil "#constant-~A" name)))
                           (a* url "constant-reference" recurse)))
                        (:figref
                         (break "should not happen")
                         (let* ((name (node-name node))
                                (url  (format nil "#figure-~A" name)))
                           (a* url "figure-reference" recurse)))
                        (:miscref      ; TODO not sure what this means
                         (break "should not happen")
                         (let* ((name (node-name node))
                                (url  (format nil "#misc-~A" name)))
                           (a* url "misc-reference" recurse)))



                        (:code (cxml:with-element "pre"
                                 (cxml:with-element "code"
                                   (cxml:unescaped (format-code content)))))
                        (:math
                         (cxml:text (if *math?* "$" "\\("))
                         (let ((*math?* t))
                           (funcall recurse :function (a:curry #'visit :math)))
                         (cxml:text (if *math?* "$" "\\)")))
                        (:math-display
                         (cxml:text "\\[ ")
                         (let ((*math?* t))
                           (funcall recurse :function (a:curry #'visit :math)))
                         (cxml:text " \\]"))

                        (:symbol
                         (render-name-node builder node))
                        (:param (span "parameter" recurse))
                        (:keyword
                         (span "keyword" (lambda ()
                                           (cxml:text ":")
                                           (funcall recurse))))
                        (:bnf-rule
                         (cxml:with-element "tr"
                           (cxml:with-element "td"
                             (funcall recurse :relations '((:name . 1))))
                           (cxml:with-element "td"
                             (cxml:text "::="))
                           (cxml:with-element "td"
                             (funcall recurse :relations '((:element . *))))))
                        (:lambda-list-keyword ; TODO generate links
                         (span "lambda-list-keyword"
                               (lambda ()
                                 (cxml:text (format nil "&~(~A~)"
                                                    (getf (bp:node-initargs builder node) :which))))))
                        (:specialized-parameter
                         (span "specialized-parameter"
                               (lambda ()
                                 (cxml:text "(")
                                 (span "name" (a:curry recurse :relations '((:name . 1))))
                                 (cxml:unescaped "&nbsp;")
                                 (funcall recurse :relations '((:spealizer . 1)) )
                                 #+no (let* ((class (bp:node-relation builder '(:specializer . 1) node))
                                        (name  (dpans-conversion.transform::evaluate-to-string builder class)) ; TODO  repeated in TYPEREF
                                        (url   (format nil "#type-~A" name)))
                                   (unless name (break "~A" node))
                                   (a* url "type-reference" (a:curry recurse :relations '((:specializer . 1)))))
                                 (cxml:text ")"))))
                        (:call-syntax
                         (ecase which
                           (:special-operator
                            (span "special-operator-definition"
                                  (lambda ()
                                    (a:curry recurse :relations '((:name . *)))
                                    (cxml:text " ")
                                    (span "lambda-list"   (a:curry recurse :relations '((:argument . *))))
                                    (cxml:unescaped "&nbsp;")
                                    (cxml:text "\\( \\rightarrow \\)")
                                    (cxml:unescaped "&nbsp;")
                                    (if (member :return-value relations :key #'car)
                                        (span "return-values" (a:curry recurse :relations '((:return-value . *))))
                                        (cxml:text "|"))))
                            (br))
                           ((:function :accessor)
                            (map nil (lambda (name)
                                       (span "function-definition"
                                             (lambda ()
                                               (render-name-node builder name)
                                               (cxml:text " ")
                                               (span "lambda-list"   (a:curry recurse :relations '((:argument . *))))
                                               (cxml:unescaped "&nbsp;")
                                               (cxml:text "\\( \\rightarrow \\)")
                                               (cxml:unescaped "&nbsp;")
                                               (if (member :return-value relations :key #'car)
                                                   (span "return-values" (a:curry recurse :relations '((:return-value . *))))
                                                   (cxml:text "|"))))
                                       (br))
                                 (bp:node-relation builder '(:name . *) node))
                            (when (eq which :accessor)
                              (map nil (lambda (name)
                                         (span "function-definition"
                                               (lambda ()
                                                 (cxml:text "(setf (")
                                                 (render-name-node builder name)
                                                 (cxml:unescaped "&nbsp;")
                                                 (span "lambda-list"   (a:curry recurse :relations '((:argument . *))))
                                                 (cxml:text ")")
                                                 (cxml:unescaped "&nbsp;")
                                                 (span "new-value" (a:curry recurse :relations '((:new-value . 1))))))
                                         (br))
                                   (bp:node-relation builder '(:name . *) node))))
                           (:generic-function
                            (span "function-definition"
                                  (lambda ()
                                    (a:curry recurse :relations '((:name . *)))
                                    (cxml:text " ")
                                    (span "lambda-list"   (a:curry recurse :relations '((:argument . *))))
                                    (cxml:unescaped "&nbsp;")
                                    (cxml:text "\\( \\rightarrow \\)")
                                    (cxml:unescaped "&nbsp;")
                                    (if (member :return-value relations :key #'car)
                                        (span "return-values" (a:curry recurse :relations '((:return-value . *))))
                                        (cxml:text "|"))))
                            (br))
                           (:method
                               (span "method"
                                (lambda ()
                                  (a:curry recurse :relations '((:name . *)))
                                  (span "lambda-list" (a:curry recurse :relations '((:argument . *))))))
                             (br))
                           (:macro
                            (span "function-definition"
                                  (lambda ()
                                    (a:curry recurse :relations '((:name . 1)))
                                    (cxml:text " ")
                                    (span "lambda-list" (a:curry recurse :relations '((:argument . *))))
                                    (cxml:unescaped "&nbsp;")
                                    (cxml:text "\\( \\rightarrow \\)")
                                    (cxml:unescaped "&nbsp;")
                                    (if (member :return-value relations :key #'car)
                                        (span "return-values" (a:curry recurse :relations '((:return-value . *))))
                                        (cxml:text "|"))))
                            (br))
                           (:type
                            (span "type-definition"
                                  (lambda ()
                                    (a:curry recurse :relations '((:name . 1)))
                                    (cxml:text " ")
                                    (span "lambda-list" (a:curry recurse :relations '((:element . *)))))) ; TODO relation name
                            (br))
                           (:setf
                            (map nil (lambda (name)
                                       (span "setf-definition"
                                             (lambda ()
                                               (cxml:text "(setf (") ; TODO
                                               (span "name" (lambda ()
                                                              (cxml:text (dpans-conversion.transform::evaluate-to-string
                                                                          builder name))))
                                               (cxml:text " ")
                                               (span "lambda-list" (a:curry recurse :relations '((:argument . *))))
                                               (cxml:text ") ")
                                               (span "new-value" (a:curry recurse :relations '((:new-value . 1))))
                                               (cxml:text ")")))
                                       (br))
                                 (bp:node-relation builder '(:name . *) node)))))

                        (:index)        ; drop index stuff here

                        ;; "Component"
                        (:ftype )
                        (:none (span "none" (lambda () (cxml:text "None"))))
                        (:part
                         (let* ((name (bp:node-relation builder '(:name . 1) node))
                                (name (dpans-conversion.transform::evaluate-to-string
                                       builder name)))
                           (flet ((do-it ()
                                    (cxml:with-element "dl" ; TODO one dl for all parts?
                                      (cxml:with-element "dt"
                                        (class-attribute "label")
                                        (funcall recurse :relations '((:name . 1))))
                                      (cxml:with-element "dd"
                                        (funcall recurse :relations '((:element . *)))))))
                             (if (find-if (a:rcurry #'a:starts-with-subseq name)
                                          '("Note" "Example" "Pronunciation" "See Also"))
                                 (removable-text #'do-it)
                                 (do-it)))))
                        (:component
                         (let* ((names     (bp:node-relation builder '(:name . *) node))
                                (ftype     (node-name (find-child-of-kind builder :ftype node)))
                                (namespace (dpans-conversion.transform::namespace<-ftype ftype)))
                           (format t "~V@TGenerating component ~{~A~^, ~}~%"
                                   (* 2 (length file-stack))
                                   (map 'list (a:curry #'transform::evaluate-to-string builder) names))
                           (br)
                           (div "component"
                                (lambda ()
                                  (div "header"
                                       (lambda ()
                                         (map nil (lambda (name next-name)
                                                    (multiple-value-bind (name setf?)
                                                        (dpans-conversion.transform::evaluate-to-string builder name)
                                                      (cxml:with-element "a"
                                                        (cxml:attribute "id" (format nil "~(~A~)-~A"
                                                                                     namespace
                                                                                     name))
                                                        (cxml:text " ")) ; HACK
                                                      (render-name name setf?)
                                                      (when next-name
                                                        (cxml:text ", "))))
                                              names (append (rest names) '(nil)))
                                         (span "ftype" (lambda () (cxml:text ftype)))))
                                  (funcall recurse :relations '((:element . *)))))
                           (br)))
                        ((:dash
                          :issue-annotation :editor-note :reviewer-note
                          :file :title :sub-title :chapter :section
                          :collection :output-file :issue
                          :reference
                          :item-list :list-item
                          :enumeration-list :enumeration-item
                          :definition-list :definition-item
                          :table :header :row :cell
                          :issue-reference)
                         (apply #'transform:transform-node transform recurse relation relation-args node kind relations initargs))
                        #+no (:issue-annotation
                         (div "issue-annotation"
                              (lambda ()
                                (span "issue-reference"
                                      (lambda ()
                                        (cxml:text "Issue: ")
                                        (cxml:text name)
                                        #+old (let ((name (dpans-conversion.transform::evaluate-to-string
                                                           builder (bp:node-relation builder '(:name . 1) node) )))
                                                (cxml:text name))))
                                (funcall recurse :relations '(:element)))))
                        #+no (:editor-note
                         (tooltip "editor-note" "editor-note-tooltip"
                                  (lambda ()
                                    (span "editor" (lambda () (cxml:text editor)))
                                    (cxml:text ": ")
                                    (cxml:text content))
                                  (lambda () (cxml:text "‣"))
                                  :element 'span))
                        #+no (:reviewer-note
                         (tooltip "reviewer-note" "reviewer-note-tooltip"
                                  (lambda ()
                                    (when reviewer
                                      (span "reviewer" (lambda () (cxml:text reviewer))))
                                    (cxml:text ": ")
                                    (cxml:text content))
                                  (lambda () (cxml:text "‣"))
                                  :element 'span))
                        ;; Structure

                        (:non-breaking-space (cxml:unescaped "&nbsp;")) ; TODO
                        (:paragraph-break (cxml:with-element "br"))
                        ;; Tables
                        (:define-figure
                         (break "should not happen")
                         (let* ((id      (node-name node))
                                (anchor  (format nil "figure-~A" id)))
                           (cxml:with-element "a"
                             (cxml:attribute "id" anchor)
                             (cxml:text " ")))) ; HACK
                        (:row-terminator
                         (span "error" (lambda ()
                                         (cxml:text "&"))))
                        ((:displaytwo :displaythree :displayfour :displayfive)
                         (break "should not happen")
                         (cxml:with-element "table"
                           #+no (let* ((id-node (find-child-of-kind builder :define-figure node))
                                       (id      (when id-node
                                                  (node-name id-node)))
                                       (anchor  (format nil "figure-~A" id)))
                                  (when id (break))
                                  (cxml:attribute "id" anchor))
                           ;; TODO caption and header
                           (funcall recurse :relations '(:row)
                                            :function  (a:curry #'visit :code))))
                        ((:showtwo :showthree)
                         (break "should not happen")
                         (cxml:with-element "table"
                           #+no (let* ((id-node (find-child-of-kind builder :define-figure node))
                                       (id      (when id-node
                                                  (node-name id-node)))
                                       (anchor  (format nil "figure-~A" id)))
                                  (when id (break))
                                  (cxml:attribute "id" anchor))
                           ;; TODO caption and header
                           (funcall recurse :relations '(:row))))
                        ((:tablefigtwo :tablefigthree :tablefigfour :tablefigsix)
                         (break "should not happen")
                         (cxml:with-element "table"
                           #+no (let* ((id-node (find-child-of-kind builder :define-figure node))
                                       (id      (when id-node
                                                  (node-name id-node)))
                                       (anchor  (format nil "figure-~A" id)))
                                  (cxml:attribute "id" anchor))
                           ;; TODO caption
                           (cxml:with-element "thead"
                             (funcall recurse :relations '(:header)))
                           (cxml:with-element "tbody"
                             (funcall recurse :relations '(:row)))))
                        (:header
                         (break "should not happen")
                         (cxml:with-element "th" (funcall recurse)))
                        (:row
                          (break "should not happen")
                          (cxml:with-element "tr" (funcall recurse)))
                        (:cell
                          (break "should not happen")
                          (cxml:with-element "td"
                            (a:when-let ((span (getf initargs :span)))
                              (cxml:attribute "colspan" (princ-to-string span)))
                            (case context
                              (:code
                               (cxml:with-element "code"
                                 (funcall recurse)))
                              (t
                               (funcall recurse)))))

                        (:tabletwo
                         (cxml:with-element "dl"
                           (funcall recurse)))
                        (:entry
                         (cxml:with-element "dt"
                           (funcall recurse :relations '((:term . 1))))
                         (cxml:with-element "dd"
                           (funcall recurse :relations '((:definition . 1)))))

                        ;; Glossary
                        (:gentry
                         (let ((term (node-name node)))
                           (format t "~V@TProcessing glossary entry ~A~%"
                                   (* 2 (length file-stack)) term)
                           (unless (equal term "case")
                             (div* "glossary-entry" anchor
                                   (lambda ()
                                     (span "term" (a:curry recurse :relations '((:name . 1))))
                                     (funcall recurse :relations '(:body)))))))
                        ;; Should not happen
                        (:column-separator
                         (span "error" (lambda () (cxml:text "&"))))
                        ;;
                        (:block
                            ;; TODO collect runs of bnf-rule
                            (cond (*math?*
                                   (cxml:text "{")
                                   (funcall recurse)
                                   (cxml:text "}"))
                                  ((some (lambda (child)
                                           (eq (bp:node-kind builder child) :bnf-rule))
                                         (bp:node-relation builder '(:element . *) node))
                                   (cxml:with-element "table"
                                     (cxml:attribute "class" "bnf")
                                     (funcall recurse)))
                                  (t
                                   (funcall recurse))))
                        ((:hbox :vbox :vtop)
                         (cond (*math?*
                                (cxml:text (format nil "\\~(~A~)" kind))
                                (funcall recurse :relations '((:element . *))))
                               (t
                                (funcall recurse :relations '((:element . *))))))

                        (:halign
                         (break "should not happen")
                         (funcall recurse :relations '((:element . *))))

                        (:bracket-group
                         (funcall recurse))

                        (:input
                         (funcall recurse :relations '((:file . bp:?))))

                        (:if
                         (funcall recurse :relations '((:consequent . *))))

                        (:if-case
                         (span "error" (lambda () (cxml:text (let ((*print-level* 2) (*print-circle* t))
                                                               (format nil "if-case not implemented: ~S" node))))))
                        (:other-command-application
                         (span "error" (lambda () (cxml:text (let ((*print-level* 2) (*print-circle* t))
                                                               (format nil "unexpanded macro: ~S" node))))))

                        ;; Ignored
                        ((:comment :define-section :assignment :font :chardef :mathchardef
                          :newif :newskip :new :counter-definition :setbox :global :catcode
                                   :advance :register-read))))
                 (pop stack))))
      (bp:walk-nodes builder #+no (bp:peeking #'peek (a:curry #'visit :top)) (a:curry #'visit :top) tree))))
