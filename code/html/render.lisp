(cl:in-package #:dpans-conversion.html)

;;;

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
             (funcall-or-insert-text content-continuation))))

(defun removable-text (continuation)
  (tooltip "removable-text" "removable-text-tooltip"
           (lambda ()
             (cxml:text "This passage is ")
             (a "chapter-1.xhtml#section-RemovableText" ; TODO do this properly
                "removable text")
             (cxml:text " which is not formally part of the standard."))
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

(defun render-name (name setf?)
  (when setf?
    (cxml:text "(setf")
    (nbsp))
  (cxml:text name)
  (when setf?
    (cxml:text ")")))

(defun render-name-node (builder name)
  (multiple-value-bind (name setf?)
      (dpans-conversion.transform::evaluate-to-string builder name)
    (span "name" (lambda () (render-name name setf?)))))

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
                        #+no (:splice
                              (funcall recurse))
                        (:argument
                         (span "error" (format nil "unresolved macro argument ~S" node)))
                        ;; Typographic Markup
                        #+no (:chunk
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
                         (let* ((name (transform::node-name* node))
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
                                 (let ((name (bp:node-relation builder '(:name . 1) node)))
                                   (cxml:text "(")
                                   (render-name-node builder name)
                                   (nbsp)
                                   (funcall recurse :relations '((:spealizer . 1)) )
                                   #+no (let* ((class (bp:node-relation builder '(:specializer . 1) node))
                                               (name  (dpans-conversion.transform::evaluate-to-string builder class)) ; TODO  repeated in TYPEREF
                                               (url   (format nil "#type-~A" name)))
                                          (unless name (break "~A" node))
                                          (a* url "type-reference" (a:curry recurse :relations '((:specializer . 1)))))
                                   (cxml:text ")")))))
                        (:call-syntax
                         (ecase which
                           (:special-operator
                            (span "special-operator-definition"
                                  (lambda ()
                                    (let ((name (bp:node-relation builder '(:name . 1) node)))
                                      (render-name-node builder name)
                                      (nbsp)
                                      (span "lambda-list" (a:curry recurse :relations '((:argument . *))))
                                      (nbsp)
                                      (cxml:text "→")
                                      (nbsp)
                                      (if (member :return-value relations :key #'car)
                                          (span "return-values" (a:curry recurse :relations '((:return-value . *))))
                                          (cxml:text "|")))))
                            (br))
                           ((:function :accessor) ; TODO accessor should be a table with rows foo | (setf foo)
                            (map nil (lambda (name)
                                       (span "function-definition"
                                             (lambda ()
                                               (render-name-node builder name)
                                               (nbsp)
                                               (span "lambda-list" (a:curry recurse :relations '((:argument . *))))
                                               (nbsp)
                                               (cxml:text "→")
                                               (nbsp)
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
                                                 (nbsp)
                                                 (span "lambda-list" (a:curry recurse :relations '((:argument . *))))
                                                 (cxml:text ")")
                                                 (nbsp)
                                                 (span "new-value" (a:curry recurse :relations '((:new-value . 1))))))
                                         (br))
                                   (bp:node-relation builder '(:name . *) node))))
                           (:generic-function
                            (span "function-definition"
                                  (lambda ()
                                    (let ((name (bp:node-relation builder '(:name . 1) node)))
                                      (render-name-node builder name)
                                      (nbsp)
                                      (span "lambda-list"   (a:curry recurse :relations '((:argument . *))))
                                      (nbsp)
                                      (cxml:text "→")
                                      (nbsp)
                                      (if (member :return-value relations :key #'car)
                                          (span "return-values" (a:curry recurse :relations '((:return-value . *))))
                                          (cxml:text "|")))))
                            (br))
                           (:method
                               (span "method"
                                (lambda ()
                                  (let ((name (bp:node-relation builder '(:name . 1) node)))
                                    (render-name-node builder name)
                                    (nbsp)
                                    (span "lambda-list" (a:curry recurse :relations '((:argument . *)))))))
                             (br))
                           (:macro
                            (span "function-definition"
                                  (lambda ()
                                    (let ((name (bp:node-relation builder '(:name . 1) node)))
                                      (render-name-node builder name)
                                      (nbsp)
                                      (span "lambda-list" (a:curry recurse :relations '((:argument . *))))
                                      (nbsp)
                                      (cxml:text "→")
                                      (nbsp)
                                      (if (member :return-value relations :key #'car)
                                          (span "return-values" (a:curry recurse :relations '((:return-value . *))))
                                          (cxml:text "|")))))
                            (br))
                           (:type
                            (span "type-definition"
                                  (lambda ()
                                    (let ((name (bp:node-relation builder '(:name . 1) node)))
                                      (cxml:text "(")
                                      (render-name-node builder name)
                                      (nbsp)
                                      (span "lambda-list" (a:curry recurse :relations '((:element . *)))) ; TODO relation name
                                      (cxml:text ")"))))
                            (br))
                           (:setf
                            (map nil (lambda (name)
                                       (span "setf-definition"
                                             (lambda ()
                                               (cxml:text "(setf (") ; TODO
                                               (span "name" (lambda ()
                                                              (cxml:text (dpans-conversion.transform::evaluate-to-string
                                                                          builder name))))
                                               (nbsp)
                                               (span "lambda-list" (a:curry recurse :relations '((:argument . *))))
                                               (cxml:text ") ")
                                               (span "new-value" (a:curry recurse :relations '((:new-value . 1))))
                                               (cxml:text ")")))
                                       (br))
                                 (bp:node-relation builder '(:name . *) node)))))

                        ((:dash :subscript :superscript
                                :issue-annotation :editor-note :reviewer-note
                          :file :title :sub-title :chapter :section
                                :collection :output-file :issue
                          :reference
                                :item-list :list-item
                                :enumeration-list :enumeration-item
                                :definition-list :definition-item
                                :table :header :row :cell
                                :figure
                                :issue-reference
                          :component :part :none :ftype
                          :paragraph-break :non-breaking-space
                          :math :math-display :over
                          :other-command-application
                                :splice :chunk :block
                                :listing :syntax :index)
                         (apply #'transform:transform-node transform recurse relation relation-args node kind relations initargs))
                        ;; Glossary
                        (:gentry
                         (let ((term (node-name node)))
                           (format t "~V@TProcessing glossary entry ~A~%"
                                   (* 2 (length file-stack)) term)
                           (unless (equal term "case")
                             (p* "glossary-entry" anchor
                                 (lambda ()
                                   (span "term" (a:curry recurse :relations '((:name . 1))))
                                   (funcall recurse :relations '(:body)))))))
                        ;; Should not happen
                        (:column-separator
                                        ; TODO (break "should not happen")
                         (span "error" "&"))
                        ;;
                        (:block
                            (break)
                            ;; TODO collect runs of bnf-rule
                            (cond (*math?*
                                   ; (cxml:text "{")
                                   (funcall recurse)
                                   ; (cxml:text "}")
                                   )
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

                        (:input         ; TODO should be dropped
                         (funcall recurse :relations '((:file . bp:?))))

                        (:if
                         (funcall recurse :relations '((:consequent . *))))

                        (:if-case
                         (span "error" (let ((*print-level* 2) (*print-circle* t))
                                         (format nil "if-case not implemented: ~S" node))))
                        ;; Ignored
                        ((:comment :define-section :assignment :font :chardef :mathchardef
                          :newif :newskip :new :counter-definition :setbox :global :catcode
                                   :advance :register-read)
                         (break "should not happen"))))
                 (pop stack))))
      (bp:walk-nodes builder #+no (bp:peeking #'peek (a:curry #'visit :top)) (a:curry #'visit :top) tree))))
