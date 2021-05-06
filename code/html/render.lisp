(cl:in-package #:dpans-conversion.html)

;;;

(defun format-code (code)
  (with-output-to-string (stream)
    (eclector.examples.highlight:highlight
     code                          ; :package (find-package #:common-)
     :client  (eclector.examples.highlight::make-minimal-client
               :stream stream))))

;;;

(defun br ()
  (cxml:with-element "br"))

(defun span (class continuation)
  (cxml:with-element "span"
    (cxml:attribute "class" class)
    (funcall continuation)))

(defun span* (class id continuation)
  (cxml:with-element "span"
    (cxml:attribute "class" class)
    (cxml:attribute "id" id)
    (funcall continuation)))

(defun div (class continuation)
  (cxml:with-element "div"
    (cxml:attribute "class" class)
    (funcall continuation)))

(defun div* (class id continuation)
  (cxml:with-element "div"
    (cxml:attribute "class" class)
    (cxml:attribute "id" id)
    (funcall continuation)))

(defun a (url continuation)
  (cxml:with-element "a"
    (cxml:attribute "href" url)
    (funcall continuation)))

(defun a* (url class continuation)
  (cxml:with-element "a"
    (cxml:attribute "href" url)
    (cxml:attribute "class" class)
    (funcall continuation)))

(defun find-child-of-kind (builder kind node)
  (let ((children (bp:node-relation builder :element node)))
    (find-if (lambda (child)
               (eq (bp:node-kind builder child) kind))
             children)))

(defun evaluate-to-string (builder node)
  (labels ((rec (node)
             (case (bp:node-kind builder node)
               (:word
                (return-from evaluate-to-string
                  (getf (bp:node-initargs builder node) :content)))
               (:symbol
                (return-from evaluate-to-string
                  (getf (bp:node-initargs builder node) :name)))
               (t
                (map nil #'rec (bp:node-relation builder :element node))))))
    (rec node)))

(defun node-name (node)
  (let* ((builder 'list)
         (name    (bp:node-relation builder '(:name . 1) node)))
    (evaluate-to-string builder name)))

(defun expand (builder body macro-level arguments)
  (labels ((visit (recurse relation relation-args node kind relations
                   &rest initargs &key level number &allow-other-keys)
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
                (bp:make+finish-node+relations
                 builder kind initargs
                 (map 'list (lambda (relation)
                              (multiple-value-bind (relation* cardinality)
                                  (bp:normalize-relation relation)
                               (ecase cardinality
                                 (1 (list '1 relation (first (funcall recurse :relations (list relation)))))
                                 (* (list '* relation (first (funcall recurse :relations (list relation))))))))
                      relations))))))
    (bp:walk-nodes builder #'visit body)))

(defvar *math?* nil) ; TODO hack

(defun render-to-file (tree file environment &key (use-mathjax        t)
                                                  (use-sidebar        t)
                                                  (debug-expansion    nil)
                                                  (modify-environment nil))
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
               (pop environment-stack)
               (unless modify-environment
                 (pop file-stack)))
             (peek (builder relation relation-args node)
               (cond ((not (eq (bp:node-kind builder node) :other-command-application))
                      t)
                     (t
                      (let* ((initargs  (bp:node-initargs builder node))
                             (name      (getf initargs :name))
                             (arguments (bp:node-relation builder '(:argument . *) node)))
                        (a:if-let ((macro (env:lookup name :macro (first environment-stack)
                                                           :if-does-not-exist nil)))
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
                                        ; (list (bp:node (builder :word :content "{")))
                                        ; (bp:node-relation builder :argument node)
                                        ; (list (bp:node (builder :word :content "}")))
                                                           )))))
                            (when debug-expansion
                              (format t "  Expanded ~A[~S] -> ~S~%" name arguments expansion))
                            (values (bp:node (builder :splice :expansion-of node)
                                      (* :element expansion))
                                    :splice '() '(:element)))
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
                                                "eject" "break" "vtop"
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
                                 nil)))))))
             (visit
                 (context recurse relation relation-args node kind relations
                  &rest initargs
                  &key source
                       name
                       content
                       filename
                  &allow-other-keys)
               (unwind-protect
                    (progn
                      (when (> (length stack) 100)
                        (break))
                      (push (if (eq kind :splice)
                                (getf (bp:node-initargs builder (getf (bp:node-initargs builder node) :expansion-of)) :name)
                                kind)
                            stack)
                      (ecase kind
                        (:file
                         (a:with-output-to-file (stream file
                                                        :element-type '(unsigned-byte 8)
                                                        :if-exists :supersede)
                           (push-file filename)
                           (cxml:with-xml-output (cxml:make-octet-stream-sink stream :omit-xml-declaration-p t
                                                                              )
                             (cxml:unescaped "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
                                        ; (cxml:unescaped "<!DOCTYPE html>")
                             (cxml:with-element "html"
                               (cxml:attribute "xmlns" "http://www.w3.org/1999/xhtml")
                               (cxml:with-element "head"
                                 (cxml:with-element "link"
                                   (cxml:attribute "rel" "stylesheet")
                                   (cxml:attribute "type" "text/css")
                                   (cxml:attribute "href" "style.css"))
                                 (when use-mathjax
                                   (cxml:with-element "script"
                                     (cxml:attribute "src" "https://polyfill.io/v3/polyfill.min.js?features=es6")
                                     (cxml:text " "))
                                   (cxml:with-element "script"
                                     (cxml:attribute "id" "MathJax-script")
                                     (cxml:attribute "src" "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
                                     (cxml:text " ")))
                                 (when use-sidebar
                                   (cxml:with-element "link"
                                     (cxml:attribute "rel" "stylesheet")
                                     (cxml:attribute "href" "https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta3/dist/css/bootstrap.min.css")
                                     (cxml:attribute "integrity" "sha384-eOJMYsd53ii+scO/bJGFsiCZc+5NDVN2yr8+0RDqr0Ql0h+rP48ckxlpbzKgwra6")
                                     (cxml:attribute "crossorigin" "anonymous"))
                                   (cxml:with-element "link"
                                     (cxml:attribute "rel" "stylesheet")
                                     (cxml:attribute "href" "https://cdn.rawgit.com/afeld/bootstrap-toc/v1.0.1/dist/bootstrap-toc.min.css"))))
                               (cxml:with-element "body"
                                 (cxml:attribute "data-spy" "scroll")
                                 (cxml:attribute "data-target" "#toc")
                                 (when use-sidebar
                                   (cxml:with-element "script"
                                     (cxml:attribute "src" "https://code.jquery.com/jquery-3.6.0.min.js")
                                     (cxml:text " "))
                                   (cxml:with-element "script"
                                     (cxml:attribute "src" "https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta3/dist/js/bootstrap.bundle.min.js")
                                     (cxml:attribute "integrity" "sha384-JEW9xMcG8R+pH31jmWH6WWP0WintQrMb4s7ZOdauHnUtxwoG2vI5DkLtS3qm9Ekf")
                                     (cxml:attribute "crossorigin" "anonymous")

                                     (cxml:text " "))
                                   (cxml:with-element "script"
                                     (cxml:attribute "src" "https://cdn.rawgit.com/afeld/bootstrap-toc/v1.0.1/dist/bootstrap-toc.min.js")
                                     (cxml:text " ")))
                                 (cxml:with-element "div"
                                   (cxml:attribute "class" "container")
                                   (cxml:with-element "div"
                                     (cxml:attribute "class" "row")
                                     (cxml:with-element "div"
                                       (cxml:attribute "class" "col-sm-3")
                                       (cxml:with-element "nav"
                                         (cxml:attribute "id" "toc")
                                         (cxml:attribute "data-toggle" "toc")
                                         (cxml:attribute "class" "sticky-top")
                                         (cxml:text " ")))
                                     (cxml:with-element "div"
                                       (cxml:attribute "class" "col-sm-9")
                                       (funcall recurse)))))))))
                        (:included-file
                         (push-file filename)
                         (unwind-protect
                              (with-simple-restart (continue "Skip included file ~S" filename)
                                (funcall recurse))
                           (pop-file)))
                        ;; Evaluation
                        (:definition
                         (format t "~V@TDefining macro ~S~%" (* 2 (length file-stack)) name)
                         (setf (env:lookup name :macro (first environment-stack)) node))
                        (:splice
                         (funcall recurse))
                        (:argument
                         (span "error" (lambda () (cxml:text (format nil "unresolved macro argument ~S" node)))))
                        ;; Typographic Markup
                        (:word
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

                        ((:list-item :enumeration-item)
                         (cxml:with-element "li"
                           (funcall recurse :relations '(:body))))
                        (:item-list
                         (cxml:with-element "ul" (funcall recurse)))
                        (:enumeration-list
                         (cxml:with-element "ol" (funcall recurse)))
                        (:definition-item
                         (cxml:with-element "dt"
                           (funcall recurse :relations '(:key)))
                         (cxml:with-element "dd"
                           (funcall recurse :relations '(:body))))
                        (:definition-list
                         (cxml:with-element "dl" (funcall recurse)))
                        ;; Semantic Markup
                        (:term
                         (let* ((name (node-name node))
                                (url  (format nil "#term-~A" name)))
                           (a* url "term" recurse)))
                        (:newterm (span "newterm" recurse))
                        (:newtermidx (span "newterm" (a:curry recurse :relations '((:name . 1)))))

                        (:secref
                         (let* ((name (node-name node))
                                (url  (format nil "#section-~A" name)))
                           (a* url "section-reference" recurse)))

                        (:keyref
                         (let* ((name (node-name node))
                                (url  (format nil "#lambda-list-keyword-~A" name)))
                           (unless name (break "~A" node))
                           (a* url "lambda-list-keyword-reference" recurse)))
                        (:typeref
                         (let* ((name (node-name node))
                                (url  (format nil "#type-~A" name)))
                           (unless name (break "~A" node))
                           (a* url "type-reference" recurse)))
                        (:declref
                         (let* ((name (node-name node))
                                (url  (format nil "#declaration-~A" name)))
                           (a* url "declaration-reference" recurse)))
                        (:specref
                         (let* ((name (node-name node))
                                (url  (format nil "#special-operator-~A" name)))
                           (a* url "special-operator-reference" recurse)))
                        (:funref
                         (let* ((name (node-name node))
                                (url  (format nil "#function-~A" name)))
                           (a* url "function-reference" recurse)))
                        (:macref
                         (let* ((name (node-name node))
                                (url  (format nil "#macro-~A" name)))
                           (a* url "macro-reference" recurse)))
                        (:varref
                         (let* ((name (node-name node))
                                (url  (format nil "#variable-~A" name)))
                           (a* url "variable-reference" recurse)))
                        (:conref
                         (let* ((name (node-name node))
                                (url  (format nil "#constant-~A" name)))
                           (a* url "constant-reference" recurse)))
                        (:figref
                         (let* ((name (node-name node))
                                (url  (format nil "#figure-~A" name)))
                           (a* url "figure-reference" recurse)))
                        (:miscref      ; TODO not sure what this means
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
                        (:special-operator-definition
                         (span "special-operator-definition"
                               (lambda ()
                                 (span "name"          (a:curry recurse :relations '((:name . 1))))
                                 (span "lambda-list"   (a:curry recurse :relations '((:argument . *))))
                                 (cxml:unescaped "&nbsp;")
                                 (cxml:text "\\( \\rightarrow \\)")
                                 (cxml:unescaped "&nbsp;")
                                 (if (member :return-value relations :key #'car)
                                     (span "return-values" (a:curry recurse :relations '((:return-value . *))))
                                     (cxml:text "|"))))
                         (br))
                        (:function-definition
                         (map nil (lambda (name)
                                   (span "function-definition"
                                         (lambda ()
                                           (span "name"          (lambda ()
                                                                   (cxml:text (evaluate-to-string builder name))))
                                           (span "lambda-list"   (a:curry recurse :relations '((:argument . *))))
                                           (cxml:unescaped "&nbsp;")
                                           (cxml:text "\\( \\rightarrow \\)")
                                           (cxml:unescaped "&nbsp;")
                                           (if (member :return-value relations :key #'car)
                                               (span "return-values" (a:curry recurse :relations '((:return-value . *))))
                                               (cxml:text "|"))))
                                    (br))
                              (bp:node-relation builder '(:name . *) node)))
                        (:macro-definition
                         (span "function-definition"
                               (lambda ()
                                 (span "name"          (a:curry recurse :relations '((:name . 1))))
                                 (span "lambda-list"   (a:curry recurse :relations '((:argument . *))))
                                 (cxml:unescaped "&nbsp;")
                                 (cxml:text "\\( \\rightarrow \\)")
                                 (cxml:unescaped "&nbsp;")
                                 (if (member :return-value relations :key #'car)
                                     (span "return-values" (a:curry recurse :relations '((:return-value . *))))
                                     (cxml:text "|"))))
                         (br))

                        (:type-definition
                         (span "type-definition"
                               (lambda ()
                                 (span "name"        (a:curry recurse :relations '((:name . 1))))
                                 (span "lambda-list" (a:curry recurse :relations '((:element . *)))))) ; TODO relation name
                         (br))

                        (:index)        ; drop index stuff here

                        ;; "Component"
                        (:ftype )
                        (:none (span "none" (lambda () (cxml:text "None"))))
                        (:part
                         (cxml:with-element "dl" ; TODO one dl for all parts?
                           (cxml:with-element "dt"
                             (funcall recurse :relations '((:name . 1))))
                           (cxml:with-element "dd"
                             (funcall recurse :relations '((:element . *))))))
                        (:component
                         (let* ((names     (bp:node-relation builder '(:name . *) node))
                                (ftype     (node-name (find-child-of-kind builder :ftype node)))
                                (namespace (a:eswitch (ftype :test #'string=)
                                             ("Symbol"                    "symbol")
                                             ("Class"                     "type")
                                             ("System Class"              "type")
                                             ("Condition Type"            "type")
                                             ("Type"                      "type")
                                             ("Type Specifier"            "type")
                                             ("Macro"                     "macro")
                                             ("Local Macro"               "macro") ; TODO what?
                                             ("Function"                  "function")
                                             ("Local Function"            "function") ; TODO what?
                                             ("Accessor"                  "function")
                                             ("Standard Generic Function" "function")
                                             ("Special Operator"          "special-operator")
                                             ("Special Form"              "special-operator") ; TODO why does this exist?
                                             ("Variable"                  "variable")
                                             ("Constant Variable"         "constant")
                                             ("Declaration"               "declaration"))))
                           (format t "~V@TGenerating component ~{~A~^, ~}~%"
                                   (* 2 (length file-stack))
                                   (map 'list (a:curry #'evaluate-to-string builder) names))
                           (br)
                           (div "component"
                                (lambda ()
                                  (map nil (lambda (name next-name)
                                             (let ((name (evaluate-to-string builder name)))
                                               (cxml:with-element "a"
                                                 (cxml:attribute "id" (format nil "~A-~A"
                                                                              namespace
                                                                              name))
                                                 (cxml:text " ")) ; HACK
                                               (span "name" (lambda () (cxml:text name)))
                                               (when next-name
                                                 (cxml:text ", "))))
                                       names (append (rest names) '(nil)))
                                  (span "ftype" (lambda () (cxml:text ftype)))
                                  (funcall recurse :relations '((:element . *)))))
                           (br)))
                        (:issue
                         (funcall recurse :relations '(:element))
                         #+no (progn
                                (br)
                                (div "issue"
                                     (lambda ()
                                       (span "name" (lambda ()
                                                      (cxml:text "Issue ")
                                                      (funcall recurse :relations '(:name))))
                                       (br)
                                       (funcall recurse :relations '(:element))))
                                (br)))
                        ;; Structure
                        ((:head :head1)
                         (span "title" recurse)
                         (br))
                        ((:chapter)
                         (cxml:with-element "h1"
                           (let* ((id     (evaluate-to-string
                                           builder (bp:node-relation builder '(:name3 . 1) node)))
                                  (anchor (format nil "section-~A" id)))
                             (cxml:attribute "id" anchor))
                           (funcall recurse :relations '((:name . 1))))
                         (funcall recurse :relations '(:element)))
                        ((:section :sub-section :sub-sub-section :sub-sub-sub-section :sub-sub-sub-sub-section)
                         (cxml:with-element (ecase kind
                                              (:section "h2")
                                              (:sub-section "h3")
                                              (:sub-sub-section "h4")
                                              (:sub-sub-sub-section "h5")
                                              (:sub-sub-sub-sub-section "h6"))
                           (let* ((id-node (find-child-of-kind builder :define-section node))
                                  (id      (if id-node
                                               (node-name id-node)
                                               (remove #\Space (node-name node))))
                                  (anchor  (format nil "section-~A" id)))
                             (cxml:attribute "id" anchor))
                           (funcall recurse :relations '((:name . 1))))
                         (funcall recurse :relations '(:element)))
                        (:non-breaking-space (cxml:unescaped "&nbsp;")) ; TODO
                        (:paragraph-break (cxml:with-element "br"))
                        ;; Tables
                        ( :define-figure
                         (let* ((id      (node-name node))
                                (anchor  (format nil "figure-~A" id)))
                           (cxml:with-element "a"
                             (cxml:attribute "id" anchor)
                             (cxml:text " ")))) ; HACK
                        ((:displaytwo :displaythree :displayfour :displayfive)
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
                         (cxml:with-element "th" (funcall recurse)))
                        (:row
                         (cxml:with-element "tr" (funcall recurse)))
                        (:cell
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
                             (div* "glossary-entry" (format nil "term-~A" term)
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
                        (:hbox
                         (cond (*math?*
                                (cxml:text "\\hbox")
                                (funcall recurse))
                               (t
                                (funcall recurse))))
                        (:bracket-group
                         (funcall recurse))
                        ;; Ignored
                        ((:input :comment :define-section :editor-note :reviewer-note))))
                 (pop stack))))
      (bp:walk-nodes builder (bp:peeking #'peek (a:curry #'visit :top)) tree))))

;;; Include

(defvar *include-depth* 0)

(defun include (tree)
  (let ((builder 'list)
        (filename*))
    (labels ((reconstitute (recurse node kind relations initargs)
               (bp:make+finish-node+relations
                builder kind initargs
                (map 'list (lambda (relation)
                             (multiple-value-bind (relation* cardinality)
                                 (bp:normalize-relation relation)
                               (let ((right (first (funcall recurse :relations (list relation)))))
                                 (list cardinality relation right))))
                     relations)))
             (visit (recurse relation relation-args node kind relations
                     &rest initargs &key filename name &allow-other-keys)
               (declare (ignore relation relation-args))
               (case kind
                 ((:file :included-file)
                  (setf filename* filename)
                  (reconstitute recurse node kind relations initargs))
                 ((:input)
                  (cond ((or (member name '("setup" "setup-for-toc")
                                     :test #'equal)
                             (a:ends-with-subseq ".fig" name))
                         nil)
                        ((a:ends-with-subseq ".tc" name)
                         (with-simple-restart (continue "Skip include ~A" name)
                           (let ((*include-depth* (1+ *include-depth*)))
                             (parse-and-include (merge-pathnames (subseq name 0 (- (length name) 3)) filename*)))))
                        (t
                         (with-simple-restart (continue "Skip include ~A" name)
                           (let ((*include-depth* (1+ *include-depth*)))
                             (parse-and-include (merge-pathnames name filename*)))))))
                 (:other-command-application
                  (if (equal name "includeDictionary")
                      (let* ((argument (first (bp:node-relation builder :argument node)))
                             (name     (evaluate-to-string builder argument)))
                        (with-simple-restart (continue "Skip include ~A" name)
                          (let ((*include-depth* (1+ *include-depth*)))
                            (parse-and-include (merge-pathnames name filename*)))))
                      node))
                 (t
                  (reconstitute recurse node kind relations initargs)))))
      (bp:walk-nodes builder #'visit tree))))

(defun parse-and-include (file)
  (format t "~V@TParsing ~A~%" (* 2 *include-depth*) (pathname-name file))
  (include (dpans-conversion.parser::parse-file file :root-kind (if (zerop *include-depth*) :file :included-file))))

;;;

(defclass toc-section ()
  ((%name     :initarg  :name
              :type     (or null string)
              :reader   name)
   (%parent   :initarg  :parent
              :reader   parent
              :initform nil)
   (%children :initarg  :children
              :type     list
              :accessor children
              :initform '())))

(defmethod print-object ((object toc-section) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~{~A~^.~} ~A" (section-number object) (name object))))

(defmethod section-number ((section toc-section))
  (a:if-let ((parent (parent section)))
    (let ((position (1+ (position section (children parent)))))
      (append (section-number parent) (list position)))
    '()))

(defclass toc-chapter (toc-section)
  ((%id :initarg  :id
        :reader   id)))

(defmethod section-number ((section toc-chapter))
  (list (id section)))

(defun build-toc (files)
  (let* ((builder 'list)
         (root    (make-instance 'toc-section :name nil))
         (stack   (list root)))
    (labels ((visit (recurse relation relation-args node kind relations &rest initargs)
               (case kind
                 ((:chapter)
                  (let* ((top     (first stack))
                         (id      (getf (bp:node-initargs
                                         builder (bp:node-relation builder '(:id . 1) node))
                                        :content))
                         (name    (node-name node))
                         (section (make-instance 'toc-chapter :name   name
                                                              :parent top
                                                              :id     id)))
                    (a:appendf (children top) (list section))
                    (push section stack))
                  (funcall recurse)
                  (pop stack))
                 ((:section :sub-section :sub-sub-section :sub-sub-sub-section :sub-sub-sub-sub-section)
                  (let* ((top     (first stack))
                         (name    (node-name node))
                         (section (make-instance 'toc-section :name   name
                                                              :parent top)))
                    (a:appendf (children top) (list section))
                    (push section stack))
                  (funcall recurse)
                  (pop stack))
                 (t
                  (funcall recurse))))
             (one-file (file)
               (bp:walk-nodes builder #'visit file)))
      (map nil #'one-file files))
    (setf (children root) (sort (children root) (lambda (left right)
                                                  (cond ((notevery #'digit-char-p right)
                                                         t)
                                                        ((notevery #'digit-char-p left)
                                                         nil)
                                                        ((let ((left  (parse-integer left))
                                                               (right (parse-integer right)))
                                                           (< left right)))))
                                :key #'id))
    root))

(defun do-it (&key (use-mathjax     t)
                   (use-sidebar     nil)
                   (debug-expansion nil))
  (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
    (flet ((process-file (input output)
             (format t "Processing file ~A~%" input)
             (with-simple-restart (continue "Skip ~A" input)
               (render-to-file (dpans-conversion.parser::parse-file input) output env
                               :use-mathjax        nil
                               :modify-environment t))))
      (process-file "data/dpANS3/setup-title.tex"    "/tmp/output/setup-title.html")
      (process-file "data/dpANS3/setup-aux.tex"      "/tmp/output/setup-aux.html")
      (process-file "data/dpANS3/setup-document.tex" "/tmp/output/setup-document.html")
      (process-file "data/dpANS3/setup-terms.tex"    "/tmp/output/setup-terms.html")
      ;; (:inspect env :new-inspector? t)
      (let* ((files (loop :for input :in (append (directory "data/dpANS3/chap-0.tex")
                                                 ;; (directory "data/dpANS3/chap-[0123456789][0123456789].tex")
                                                 ;; (directory "data/dpANS3/dict-*.tex")
                                                 ;; (directory "data/dpANS3/concept-*.tex")
                                                 )
                          :collect
                             (with-simple-restart (continue "Skip ~A" input)
                               (parse-and-include input))))
             (toc   (build-toc files)))
        (:inspect (vector env
                          (architecture.builder-protocol.visualization::as-tree
                           (first files) 'list)
                          (architecture.builder-protocol.visualization::as-query
                           (first files) 'list :editor-note)
                          (architecture.builder-protocol.visualization::as-query
                           (dpans-conversion.transform::apply-transforms
                            (list ; (make-instance 'dpans-conversion.transform::strip-comments)
                                  (make-instance 'dpans-conversion.transform::expand-macros :environment env :debug-expansion '("includeDictionary"))
                                  ; (make-instance 'dpans-conversion.transform::strip-tex-commands)
                                  )
                            (first files))
                           'list
                           :editor-note)
                          toc)
                  :new-inspector? t)
        (map nil (lambda (file)
                   (let* ((filename (getf (bp:node-initargs 'list file) :filename))
                          (name     (pathname-name filename))
                          (output   (merge-pathnames (make-pathname :name name
                                                                    :type "html")
                                                     "/tmp/output/")))
                     (format t "Generating ~A~%" name)
                     (ensure-directories-exist output)
                     (with-simple-restart (continue "Skip ~A" filename)
                       (render-to-file file output env
                                       :use-sidebar     use-sidebar
                                       :use-mathjax     use-mathjax
                                       :debug-expansion debug-expansion))))
             files)))))

