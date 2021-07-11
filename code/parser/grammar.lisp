(cl:in-package #:dpans-conversion.parser)

(defgrammar dpans
  (:class   dpans-grammar)
  (:cached? nil))

(in-grammar dpans)

;;; Utilities

(defmacro with-name-string ((name-var name-node) &body body)
  (check-type name-var symbol)
  `(let ((,name-var (getf (bp:node-initargs* ,name-node) :content)))
     (check-type ,name-var string)
     ,@body))

(defvar *depth* 0)

;;; Lexical stuff

(defrule end-of-line ()
  (or #\Newline (not :any)))

(defrule whitespace/in-line+ ()
  (+ (or #\Space #\Tab)))

(defrule whitespace* ()
  (* (or #\Space #\Tab #\Newline)))

(defrule comment-line ()
    (bounds (start end)
      (seq #\% (* (<<- content (and (not (end-of-line)) :any))) (end-of-line)))
  (let ((content (coerce (nreverse content) 'string)))
    (bp:node* (:comment-line :content content :bounds (cons start end)))))

(defrule comment ()
    (bounds (start end)
      (seq (<<- lines (comment-line))
           (* (seq (? (whitespace/in-line+)) (<<- lines (comment-line))))))
  (bp:node* (:comment :bounds (cons start end))
    (* (:line . *) (nreverse lines))))

(defrule skippable () ; TODO make this inlinable
    (or #\Space #\Tab #\Newline (comment))
  nil)

(defrule skippable+ ()
    (+ (skippable))
  nil)

(defrule skippable* ()
    (* (skippable))
  nil)

(defrule terminator ()
    (and (or (skippable) (not :any))
         (seq)))

(defrule identifier ()
    (+ (<<- content (guard alpha-char-p)))
    #+no (seq (<<- content (and (not (or #\{ #\} #\\ #\. #\, #\! #\; #\# #\& #\$ #\= #\< #\> #\- #\_ #\| #\[ #\] #\^ #\" #\' #\`
                                    (terminator)))
                           :any))
         (* (<<- content (and (not (or #\{ #\} #\\ #\. #\, #\! #\; #\# #\& #\$ #\= #\< #\> #\- #\_ #\| #\[ #\] #\^ #\" #\' #\`
                                       (guard digit-char-p)
                                       (terminator)))
                              :any))))
  #+todo (when (digit-char-p (a:lastcar content)) ; only single-character like \0, maybe?
    (break))
  (coerce (nreverse content) 'string))

(defrule identifier-with-dot ()
    (+ (<<- content (or (guard alpha-char-p) #\. #\, #\! #\;)))
    #+no (seq (<<- content (and (not (or #\{ #\} #\\ #\# #\& #\$ #\= #\< #\> #\- #\_ #\| #\[ #\] #\^ #\" #\' #\`
                                    (terminator)))
                           :any))
         (* (<<- content (and (not (or #\{ #\} #\\ #\# #\& #\$ #\= #\< #\> #\- #\_ #\| #\[ #\] #\^ #\" #\' #\`
                                       (guard digit-char-p)
                                       (terminator)))
                              :any))))
  #+todo (when (digit-char-p (a:lastcar content))
    (break))
  (coerce (nreverse content) 'string))

(defrule name ()
    (bounds (start end) (seq "\\" (<- name (identifier))))
  (bp:node* (:name :content name :bounds (cons start end))))

(defrule spacing-command ()
    (seq #\\ (or #\, #\! #\: #\> #\; ; spaces
                 #\/))               ; italics correction
  nil)

(defrule tilde (environment)
    (bounds (start end) #\~)
  (if (env:lookup "~" :macro environment :if-does-not-exist nil)
      (bp:node* (:other-command-application :name "~" :bounds (cons start end)))
      (bp:node* (:non-breaking-space :bounds (cons start end)))))

(defrule mdash ()
    (bounds (start end) (seq "--" (? (<- third #\-)))) ; TODO also single dash
  (bp:node* (:dash :which  (if third :em :en)
                   :bounds (cons start end))))

(defrule escaped-character ()
    (seq #\\ (<- character (or #\\ #\@ #\= #\Space #\' #\[ #\]
                               #\Newline ; not sure about newline
                               #\% #\# #\_ #\{ #\} #\& #\$ #\. #\^ #\")))
  character)

(defrule indexed-char ()
    (seq "\\char" (skippable*) (? #\') (+ (<<- id (guard digit-char-p))))
  (let ((code (parse-integer (coerce (nreverse id) 'string) :radix 8)))
    (bp:node* (:chunk :content (string (code-char code))))))

(defrule chunk ()
    (+ (<<- characters (or (escaped-character)
                           (and (not (or #\{ #\} #\\ #\% #\& #\$ #\~ ; #\.
                                         (seq (+ #\#) (+ (guard digit digit-char-p))) ; argument
                                         (mdash)
                                         (paragraph-break))) ; TODO make non-result version
                                :any))))
  (bp:node* (:chunk :content (coerce (nreverse (remove nil characters)) 'string))))

(defrule paragraph-break ()
  (bounds (start end) (seq #\Newline (* (or #\Space #\Tab)) #\Newline))
  (bp:node* (:paragraph-break :bounds (cons start end))))

;;; Markup

(defmacro define-group (name kind open-delimiter close-delimiter)
  `(defrule ,name (environment)
       (bounds (start end)
         (seq ,open-delimiter (skippable*)
              (* (<<- elements (and (not ,close-delimiter) (element environment))))
              (skippable*) ,close-delimiter)
         (:transform (seq) nil)) ; HACK
     (bp:node* (,kind :bounds (cons start end))
       (* (:element . *) (nreverse elements)))))

(defmacro define-command (name-and-options &body arguments)
  (destructuring-bind (name &key (kind         (a:make-keyword name))
                                 (command-name (string-downcase name)))
      (a:ensure-list name-and-options)
    (let ((variables '()))
      (flet ((make-argument-expression (argument)
               (destructuring-bind (cardinality relation expression
                                    &key (open-delimiter  #\{)
                                         (close-delimiter #\}))
                   argument
                 (let ((variable (gensym (string relation))))
                   (a:appendf variables (list variable))
                   `(seq
                     ,@(flet ((one (expression)
                                `(,@(when open-delimiter
                                      `((skippable*) ,open-delimiter))
                                  ,expression
                                  ,@(when close-delimiter
                                      `((skippable*) ,close-delimiter)))))
                         (case cardinality
                           (1  (one `(<- ,variable ,expression)))
                           (1* (one `(* (<<- ,variable ,expression))))
                           (*  `((* (seq ,@(one `(<<- ,variable ,expression))))))
                           (*> (one `(<- ,variable ,expression)))
                           (t  `((* (seq ,@(one `(<<- ,variable ,expression)))
                                    ,cardinality ,cardinality)))))))))
             (make-argument-result (argument variable)
               (destructuring-bind (cardinality relation expression
                                    &key &allow-other-keys)
                   argument
                 (declare (ignore expression))
                 (case cardinality
                   (1  `(,cardinality (,relation . 1) ,variable))
                   (*> `(*            (,relation . *) ,variable))
                   (t  `(*            (,relation . *) (nreverse ,variable)))))))
        (let ((keyword (format nil "\\~A" command-name)))
          `(defrule ,name (environment)
               (bounds (start end)
                 (seq ,keyword
                      ,@(map 'list #'make-argument-expression arguments)
                      (:transform (seq) nil) ; HACK
                      ))
             (bp:node* (,kind :bounds (cons start end))
               ,@(map 'list #'make-argument-result arguments variables))))))))

(defrule verb ()
    (seq "\\verb" delimiter
         (* (<<- content (and (not delimiter) :any)))
         delimiter)
  (let ((content (coerce (nreverse content) 'string)))
    (bp:node* (:verbatim :content content))))

#+no (define-command (b :kind :bold)
  (1* :element (element environment)))

(defrule bf (environment)
    (bounds (start end)
      (seq "{\\bf" (* (<<- elements (and (not (or #\} #\&)) (element environment)))) #\}))
  (bp:node* (:bold :bounds (cons start end))
    (* :element (nreverse elements))))

(define-command (bold :kind :bold)
  (1* :element (element environment)))

#+no (define-command (i :kind :italic)
  (1* :element (element environment)))

(define-command (ital :kind :italic)
  (1* :element (element environment)))

(defrule it (environment)
    (bounds (start end)
      (seq "{\\it" (* (<<- elements (and (not #\}) (element environment)))) #\}))
  (bp:node* (:italic :bounds (cons start end))
    (* :element (nreverse elements))))

(define-command f ; "fixed", that is monospace font
  (1* :element (element environment)))

(defrule tt (environment)
    (bounds (start end)
      (seq "{\\tt" (* (<<- elements (and (not #\}) (element environment)))) #\}))
  (bp:node* (:typewriter :bounds (cons start end))
    (* :element (nreverse elements))))

(defrule rm (environment)
  (bounds (start end)
    (seq "{\\rm" (* (<<- elements (and (not #\}) (element environment)))) #\}))
  (bp:node* (:roman :bounds (cons start end))
    (* :element (nreverse elements))))

(define-group block*        :block         #\{  #\})
(define-group bracket-group :bracket-group #\[  #\])

(defrule math-group (environment)
  (bounds (start end)
    (seq/ws #\$
            (<- new-environment (:transform (seq)
                                  (env:augmented-environment
                                   environment '((:mode . :traversal)) '(:math))))
            (* (seq (<<- elements (and (not #\$) (element new-environment)))
                    (whitespace*))) ; whitespace is not significant in math mode
            #\$))
  (bp:node* (:math :bounds (cons start end))
    (* (:element . *) (nreverse elements))))

(defrule math-display (environment)
    (bounds (start end)
      (seq/ws "$$"
              (<- new-environment (:transform (seq)
                                    (env:augmented-environment
                                     environment '((:mode . :traversal)) '(:math))))
              (* (seq (<<- elements (and (not #\$) (element new-environment)))
                      (whitespace*))) ; whitespace is not significant in math mode
              "$$"
              (:transform (seq) nil))) ; HACK
  (bp:node* (:math-display :bounds (cons start end))
    (* (:element . *) (nreverse elements))))

(defrule code ()
    (bounds (start end)
      (seq "\\code" #\Newline
           (* (<<- content (and (not "\\endcode") :any)))
           "\\endcode"))
  (let ((content (coerce (nreverse content) 'string)))
    (bp:node* (:code :content content :bounds (cons start end)))))

(defun include-file (name environment)
  (let* ((filename (when name
                     (merge-pathnames name (env:lookup :current-file :traversal environment))))
         (input    (when filename
                     (a:read-file-into-string filename))))
    (when input
      (format t "Including ~A -> ~A~%" name filename)
      (let* ((include-depth   (env:lookup :include-depth :traversal environment
                                                         :if-does-not-exist 0))
             (new-include-depth (1+ include-depth))
             (new-environment (env:augmented-environment
                               environment
                               '((:current-file  . :traversal)
                                 (:include-depth . :traversal))
                               (list filename new-include-depth))))
        (%parse-tex 'list input new-environment filename new-include-depth)))))

(defrule input (environment)
    (bounds (start end)
      (seq/ws "\\input"
              (or (<- argument (argument)) ; HACK
                  (seq (<<- name :any)
                       (* (<<- name (and (not (or (skippable) #\} #\\)) :any)))))
                                        ; (terminator)
              ))
  (cond ((env:lookup :definition :traversal environment ; delay \input within \def
                                 :if-does-not-exist nil)
         (bp:node* (:input :name name :bounds (cons start end))))
        (t
         (let* ((name  (coerce (nreverse name) 'string))
                (name* (cond ((member name '("setup" "index.idx") :test #'string=)
                              nil)
                             ((string= name "setup-sections-for-toc")
                              "setup-sections")
                             ((a:ends-with-subseq "fig" name)
                              nil)
                             ((a:ends-with-subseq "tc" name)
                              (concatenate 'string (subseq name 0 (- (length name) 2)) "tex"))
                             (t
                              name)))
                (file  (include-file name* environment)))
           (when (null file)
             (format t "Not including ~A~%" name))
           (bp:node* (:input :name name :bounds (cons start end))
             (bp:? (:file . bp:?) file))))))

(defrule include-dictionary (environment)
    (bounds (start end)
      (seq/ws "\\includeDictionary" #\{
              (seq (<<- name :any)
                   (* (<<- name (and (not (or (skippable) #\} #\\)) :any))))
              #\}))
  (cond ((env:lookup :definition :traversal environment ; delay \input within \def
                                 :if-does-not-exist nil)
         (bp:node* (:input :name name :bounds (cons start end))))
        (t
         (let* ((name  (coerce (nreverse name) 'string))
                (file  (include-file name environment)))
           (bp:node* (:input :name name :bounds (cons start end))
             (bp:? (:file . bp:?) file))
           #+no (bp:node* (:section :level  1 ; TODO do this as a transform
                               :bounds (cons start end))
             (1 (:name    . 1) (bp:node* (:chunk :content "Dictionary" :bounds (cons start end))))
             (1 (:element . *) ))))))

(define-command term
  (1 :name (chunk)))

(define-command newterm
  (1 :name (chunk)))

(define-command newtermidx
  (1 :name (chunk))
  (1 :term (chunk)))

(define-command ftype
  (1 :name (element environment)))

(defrule secref ()
    (bounds (start end)
      (seq "\\secref" (or (seq #\\ (<- name (identifier)))
                          (<- name (argument)))))
  (bp:node* (:secref :bounds (cons start end))
    (1 (:name . 1) (if (stringp name)
                       (bp:node* (:chunk :content name)) ; TODO chunk is a hack
                       name))))

(defrule chapref ()
    (bounds (start end)
      (seq "\\chapref" (or (seq #\\ (<- name (identifier)))
                           (<- name (argument)))))
  (bp:node* (:secref :bounds (cons start end))
    (1 (:name . 1) (if (stringp name)
                       (bp:node* (:chunk :content name))
                       name))))

(define-command keyref ; lambda list keyword reference
  (1 :name (element environment)))

#+later (defrule simple-reference ()
    (bounds (start end)
      (seq/ws (seq "\\" (<- namespace (or (:transform "type" :typef)
                                          (:transform "decl" :declaration)
                                          (:transform "spec" :special-operator)
                                          (:transform "fun"  :function)
                                          (:transform "mac"  :macro)
                                          (:transform "var"  :variable)
                                          (:transform "con"  :constant)))
                   "ref")
              #\{ (<- name (element environment)) #\}))
  (bp:node* (:reference :namespace namespace
                        :bounds    (cons start end))
    (1 (:name . 1) name)))

(define-command typeref
  (1 :name (element environment)))

(define-command declref
  (1 :name (element environment)))

(define-command specref
  (1 :name (element environment)))

(define-command funref
  (1 :name (element environment)))

(define-command macref
  (1 :name (element environment)))

(define-command varref
  (1 :name (element environment)))

(define-command conref ; constant
  (1 :name (element environment)))

(defrule figref (environment)
    (bounds (start end)
      (seq/ws (seq "\\" (or #\f #\F) "igref")
              (<- name (or (name)
                           (element environment)))))
  (bp:node* (:figref :bounds (cons start end))
    (1 (:name . 1) name)))

(defrule miscref (environment)
    (bounds (start end)
      (seq "\\misc" (? "ref")
           (skippable*) #\{ (<- name (element environment)) (skippable*) #\}))
  (bp:node* (:miscref :bounds (cons start end))
    (1 (:name . 1) name)))

(defrule reference (environment)
  (or (chapref)
      (secref)
      (keyref environment)
      (typeref environment)
      (declref environment)
      (specref environment)
      (funref environment)
      (macref environment)
      (varref environment)
      (conref environment)
      (figref environment)
      (miscref environment)))

#+later (defrule simple-index (environment)
    (bounds (start end)
      (seq/ws (seq "\\idx" (<- namespace (or (:transform "ref"     :symbol)
                                             (:transform "keyref"  :lambda-list-keyword)
                                             (:transform "code"    :?)
                                             (:transform "kwd"     :keyword)
                                             (:transform "text"    :?)
                                             (:transform "term"    :?)
                                             (:transform "example" :constant)
                                             (:transform "packref" :?))))
              #\{ (<- name (element environment)) #\}))
  (bp:node* (:index :namespace namespace
                    :bounds    (cons start end))
    (1 (:name . 1) name)))

(macrolet
    ((define ()
       (let ((rules '()))
         (flet ((one-index (keyword
                            &key (rule-name (a:symbolicate '#:index- keyword))
                                 (command   (string-downcase keyword)))
                  (let ((string (concatenate 'string "\\idx" command))
                        (which  (a:make-keyword keyword)))
                    (push rule-name rules)
                    `(defrule ,rule-name (environment)
                       (bounds (start end)
                         (seq ,string
                              (skippable*) #\{ (<- name (element environment)) (skippable*) #\}))
                       (bp:node* (:index :which  ,which :bounds (cons start end))
                         (1 (:name . 1) name))))))
           `(progn
              ,@(map 'list #'one-index '(#:ref #:keyref #:code #:kwd
                                         #:text #:term #:example #:packref))
              (defrule index (environment)
                (or ,@(map 'list (lambda (name)
                                   `(,name environment))
                           rules))))))))
  (define))

;;;

(defmacro define-environment (name-and-options)
  (labels ((either-case (name)
             (let ((first (aref name 0))
                   (rest  (subseq name 1)))
               `((or ,(char-downcase first)
                     ,(char-upcase first))
                 ,rest)))
           (make-keyword (keyword parts)
             `(seq "\\" ,keyword ,@(a:mappend #'either-case (a:ensure-list parts)))))
    (destructuring-bind (name &key (kind         (a:make-keyword name))
                                   (keyword      (string-capitalize name))
                                   (start-string (make-keyword "begin" keyword))
                                   (end-string   (make-keyword "end"   keyword))
                                   (element      '(element environment))
                                   (name?        t))
        (a:ensure-list name-and-options)
      `(defrule ,name ( environment)
           (bounds (start end)
             (seq ,start-string ,@(when name? '(#\{ (<- name (chunk)) #\}))
                  (skippable*)
                  (* (<<- elements (and (not ,end-string)
                                        ,element)))
                  ,end-string
                  (:transform (seq) nil))) ; HACK
         (bp:node* (,kind :bounds (cons start end))
           ,@(when name? `((1 (:name . 1) name)))
           (* :element (nreverse elements)))))))

(defrule chapter (environment)
    (bounds (start end)
      (seq (seq "\\" "begin" (or #\c #\C) "hapter")
           #\{ (<- id (chunk)) #\}
           #\{ (<- name (chunk)) #\}
           #\{ (<- name2 (chunk)) #\}
           #\{ (<- name3 (chunk)) #\}
           (skippable*)
           (* (<<- elements
                   (and (not (seq "\\" "end" (or #\c #\C) "hapter")) (element environment))))
           (seq "\\" "end" (or #\c #\C) "hapter")
           (:transform (seq) nil)))
  (bp:node* (:chapter :bounds (cons start end))
    (1 (:id      . 1) id)
    (1 (:name    . 1) name)
    (1 (:name2   . 1) name2)
    (1 (:name3   . 1) name3)
    (* (:element . *) (nreverse elements))))

(define-command (head :command-name "Head" :kind :title)
  (1 :name (element environment)))
(define-command (head1 :command-name "HeadI" :kind :sub-title)
  (1* :name (element environment)))

(defrule subs ()
    (seq (<- count (:transform (seq) 0))
         (* (seq (or #\s #\S) "ub" (:transform (seq) (incf count)))))
  count)

(defrule section (environment)
    (bounds (start end)
      (seq/ws (seq "\\begin" (<- count (subs)) (or #\s #\S) "ection")
              (or (seq/ws #\{ (* (<<- name (element environment))) #\}
                          (:transform (seq) (format *trace-output* "~V@T[ section ~A~%" *depth* name) (incf *depth*))
                          (* (<<- elements (and (not (seq "\\end" (<- count (subs)) (or #\s #\S) "ection"))
                                                (element environment))))
                          (seq "\\end" (<- count (subs)) (or #\s #\S) "ection")
                          (:transform (seq) (decf *depth*) (format *trace-output* "~V@T] section ~A~%" *depth* name))
                          (:transform (seq) nil))
                  (:transform (seq) (decf *depth*) (format *trace-output* "~V@TX section ~A~%" *depth* name) (:fail)))))
  (bp:node* (:section :level  (1+ count)
                      :bounds (cons start end))
    (1 (:name    . 1) (bp:node* (:splice) ; TODO splice is a hack
                        (* (:element . *) (nreverse name))))
    (* (:element . *) (nreverse elements))))

(defrule simple-section (environment)
    (bounds (start end)
      (seq/ws (seq "\\beginSimpleChapter" (? "Left"))
              #\{ (<- name (chunk)) #\}
              (* (<<- elements (and (not "\\endSimpleChapter")
                                    (element environment))))
              "\\endSimpleChapter"
              (:transform (seq) nil)))
  (bp:node* (:section :level  1
                      :bounds (cons start end))
    (1 (:name    . 1) name)
    (* (:element . *) (nreverse elements))))

(defrule item-keyword ()
  (seq "\\item" (? "item") #\{ (or "\\bull" "--" (seq)) #\})) ; TODO empty bullet is used as block quote

(defrule list-item (environment)
    (bounds (start end)
      (seq/ws (item-keyword)
              (* (<<- body (and (not (or (seq "\\item" (? "item") #\{)#+was(item-keyword) "\\endlist"))
                                (element environment))))))
  (bp:node* (:list-item :bounds (cons start end))
    (* (:body . *) (nreverse body))))

(defrule item-list (environment)
    (bounds (start end)
      (seq/ws "\\beginlist"
              (:transform (seq) (format *trace-output* "~V@T[ item list~%" *depth*) (incf *depth*))
              (or (seq/ws
                   (* (and (not "\\endlist")
                           (seq (skippable*)
                                (<<- elements (or (issue-annotation environment)
                                                  (reviewer)
                                                  (editor-note)
                                                  (list-item environment)))
                                (skippable*))))
                   (:transform "\\endlist" nil)
                   (:transform (seq) (decf *depth*) (format *trace-output* "~V@T] item list~%" *depth*)))
                  (:transform (seq) (decf *depth*) (format *trace-output* "~V@TX item list~%" *depth*) (:fail)))))
  (bp:node* (:item-list :bounds (cons start end))
    (* :element (nreverse elements))))

(defrule enumeration-item-keyword ()
  (seq "\\item" (? "item") #\{ (+ (guard digit-char-p)) ".}"))

(defrule enumeration-item (environment)
    (bounds (start end)
      (seq/ws (enumeration-item-keyword)
              (* (<<- body (and (not (or (seq "\\item" (? "item") #\{)#+was (enumeration-item-keyword) "\\endlist")) (element environment))))))
  (bp:node* (:enumeration-item :bounds (cons start end))
    (* (:body . *) (nreverse body))))

(defrule enumeration-list (environment)
    (bounds (start end)
      (seq/ws "\\beginlist"
              (:transform (seq) (format *trace-output* "~V@T[ enum list~%" *depth*) (incf *depth*))
              (or (seq/ws (* (and (not "\\endlist")
                               (seq (skippable*)
                                    (<<- elements (or (issue-annotation environment)
                                                      (enumeration-item environment)))
                                    (skippable*))))
                       (:transform  "\\endlist" nil)
                       (:transform (seq) (decf *depth*) (format *trace-output* "~V@T] enum list~%" *depth*)))
                  (:transform (seq) (decf *depth*) (format *trace-output* "~V@TX enum list~%" *depth*) (:fail)))))
  (bp:node* (:enumeration-list :bounds (cons start end))
    (* :element (nreverse elements))))
#+no (define-environment (enumeration-list :keyword "list"
                                      :name?   nil
                                      :element (:transform
                                                (seq (skippable*) (<- item (or (issue-annotation environment) (enumeration-item environment))) (skippable*))
                                                item)))

(defrule definition-item (environment)
    (bounds (start end)
      (seq/ws (seq "\\item" (? "item")) #\{ (* (<<- keys (element environment))) #\}
              (* (<<- body (and (not (or (seq "\\item" (? "item") #\{) "\\endlist"))
                                (element environment))))))
  (bp:node* (:definition-item :bounds (cons start end))
    (* (:key  . *) (nreverse keys))
    (* (:body . *) (nreverse body))))

(defrule definition-list (environment)
    (bounds (start end)
      (seq/ws "\\beginlist"
              (:transform (seq) (format *trace-output* "~V@T[ definition list~%" *depth*) (incf *depth*))
              (or (seq/ws (* (and (not "\\endlist")
                                  (seq (skippable*)
                                       (<<- elements
                                            (or (issue-annotation environment)
                                                (definition-item environment)))
                                       (skippable*))))
                          (:transform "\\endlist" nil)
                          (:transform (seq) (decf *depth*) (format *trace-output* "~V@T] definition list ~D~%" *depth* (length elements))))
                  (seq (:transform (seq) (decf *depth*) (format *trace-output* "~V@TX definition list~%" *depth*) (:fail))
                       (:transform (seq) (if (yes-or-no-p) (:fail) (:fatal "rest of list"))))
                  )))
  (bp:node* (:definition-list :bounds (cons start end))
    (* :element (nreverse elements))))
#+no (define-environment (definition-list :keyword "list"
                                     :name?   nil
                                     :element (:transform
                                               (seq (skippable*) (<- item (or (issue-annotation environment) (definition-item environment))) (skippable*))
                                               item)))

(defrule define-section ()
    (bounds (start end)
      (seq "\\DefineSection" #\{ (<- name (chunk)) #\}))
  (bp:node* (:define-section :bounds (cons start end))
    (1 (:name . 1) name)))

(defrule define-figure ()
    (bounds (start end)
      (seq "\\DefineFigure" #\{ (<- name (chunk)) #\}))
  (bp:node* (:define-figure :bounds (cons start end))
    (1 (:name . 1) name)))

;;;

(defrule catcode (environment)
  (seq "\\catcode" (skippable*) "`\\" character
       (? #\=) (or (name)
                   (* (guard digit-char-p) 1 3)))
  (bp:node* (:catcode)
    ))

(defun (setf lookup) (new-value name namespace environment)
  (labels ((find-global (environemnt)
             (cond ((env:lookup :global? :traversal environemnt
                                :scope             :direct
                                :if-does-not-exist nil)
                    environemnt)
                   (t
                    (find-global (env:parent environemnt))))))
    (setf (env:lookup name namespace (find-global environment)) new-value)))

(defrule chardef (environment)
    (bounds (start end)
      (seq/ws "\\chardef" (<- name (name))
              #\= (or (seq #\` (escaped-character))
                      (* (guard digit-char-p) 1 3))))
  (with-name-string (name name)
    (setf (lookup name :macro environment) 0))
  (bp:node* (:chardef :bounds (cons start end))
    (1 (:name . 1) name)))

(defrule mathchardef (environment)
    (bounds (start end)
      (seq/ws "\\mathchardef" (<- name (name))
              (seq #\" (* (guard (digit-char-p 16)) 4 4)))) ; TODO code
  (with-name-string (name name)
    (setf (lookup name :math environment) 0))
  (bp:node* (:mathchardef  :bounds (cons start end))
    (1 (:name . 1) name)))

(defrule global-modifier (environment) ; TODO
    (seq/ws "\\global" (<- modified (element environment)))
  (bp:node* (:global)
    (1 (:modified . 1) modified)))

(defrule argument ()
    (bounds (start end)
      (seq (+ (<<- level #\#))
           (<- number (:transform (guard digit digit-char-p)
                        (digit-char-p digit)))))
  (bp:node* (:argument :level  (length level)
                       :number number
                       :bounds (cons start end))))

(defrule parameter ()
    (bounds (start end)
      (seq (+ (<<- level #\#))
           (<- number (:transform (guard digit digit-char-p)
                        (digit-char-p digit)))
           (* (<<- delimiter (and (not (or (seq (? (whitespace/in-line+)) #\Newline) #\# #\{)) ; TODO delimiter cannot be newline, i guess
                                  :any)))))
  (let ((delimiter (when delimiter
                     (coerce (nreverse delimiter) 'string))))
    (bp:node* (:argument :level     (length level)
                         :number    number
                         :delimiter delimiter))))

(defrule def (environment)
    (bounds (start end)
      (seq (or (seq "\\global" (skippable*) "\\def" (<- global? (:transform (seq) t))) ; TODO global should be context
               (seq "\\long" (skippable*) "\\def" (<- long (:transform (seq) t)))
               (seq "\\" (? (<- global? (or #\e #\g #\x))) "def"))
           (or (seq #\\ (<- name (identifier-with-dot))) ; TODO do we really need with dot here? there is a macro named "etc.", so probably yes
               (<- name (escaped-character)))
           (* (seq (skippable*) (or (seq #\( (<<- arguments (parameter)) #\)) ; TODO the parens are probably just delimiters
                                    (<<- arguments (parameter)))))
           (skippable*)
           (<- new-environment (:transform (seq) (env:augmented-environment environment '((:definition . :traversal)) '(t))))
           #\{ (* (<<- body (or (argument) (element new-environment)))) #\} ; TODO (argument) is basically wrong here
           (:transform (seq) nil) ; HACK work around bug
           ))
  (let ((name      (string name))
        (arguments (nreverse arguments)))
    (setf (lookup name :macro environment) (list arguments)) ; TODO not for nested definitions
    (bp:node* (:definition :kind    :def
                           :name    name
                           :global? global?
                           :bounds  (cons start end))
      (* (:argument . *) arguments)
      (* (:body     . *) (nreverse body)))))

(defrule let-macro (environment)
  (bounds (start end)
    (seq/ws (? (<- global? (:transform "\\global" t)))
            "\\let" (<- name (or (name) :any)) (? #\=)
            (or (<- node (hbox environment))
                (<- rhs (name))
                (seq #\{ (+ (<<- body (and (not (or (skippable) #\})) :any))) #\})
                (+ (<<- body (or (escaped-character)
                                 (and (not (or (skippable) #\} "\\fi")) :any))))))) ; TODO fi is a hack

  (let ((name (if (characterp name)
                  (string name)
                  (with-name-string (name name) name))))
    (setf (lookup name :macro environment) 0)

    (bp:node* (:definition :kind    :let
                           :name    name
                           :global? global?
                           :bounds  (cons start end))
      (1 (:body . *) (cond (node)
                           (body
                            (let ((body (coerce (nreverse body) 'string)))
                                 (bp:node* (:chunk :content body))))
                           (rhs
                            (let* ((initargs (bp:node-initargs* rhs))
                                   (name     (getf initargs :content))
                                   (bounds   (getf initargs :bounds)))
                              (check-type name string)
                              (bp:node* (:other-command-application :name name :bounds bounds)))))))))

(defun lookup-macro (name environment error? bounds)
  (let* ((definition? (env:lookup :definition :traversal environment
                                              :if-does-not-exist :normal))
         (mode        (env:lookup :mode :traversal environment
                                        :if-does-not-exist :normal))
         (namespace   (ecase mode
                        (:normal :macro)
                        (:math   :math)))
         (spec        (or (env:lookup name namespace environment
                                      :if-does-not-exist nil)
                          (when (eq mode :math)
                            (env:lookup name :macro environment
                                        :if-does-not-exist nil))
                          (when (and definition? (not (eq mode :math))) ; allow math macro invocation while defining macros
                            (env:lookup name :math environment
                                        :if-does-not-exist nil))
                          (when error?
                            (error 'parse-error :annotations (list (base:make-annotation (env:lookup :current-file :traversal environment)
                                                                                         bounds
                                                                                         "called here"))
                                                :message     (format nil "~@(~A~) macro ~A is not defined"
                                                                     mode name))))))
    (when spec
      (values (typecase spec
                (integer (make-list spec :initial-element 't))
                (list    (first spec)))
              t))))

(defrule user-macro-application (environment)
    (bounds (start end)
      (seq #\\ (<- argument-spec (or (:transform (<- name (identifier)) ; TODO use lookup-macro
                                       (multiple-value-bind (spec found?)
                                           (lookup-macro name environment nil (cons start (1+ start)))
                                         (unless found? (:fail))
                                         spec))
                                     (:transform (<- name (identifier-with-dot))
                                       (lookup-macro name environment t (cons start (1+ start))))))
           (* (seq (skippable*)
                   (or (and (:transform (seq)
                              (when (or (null argument-spec)
                                        (not (eq (first argument-spec) 't)))
                                (:fail)))
                            (<<- arguments (element environment)))
                       (and (:transform (seq)
                              (when (or (null argument-spec)
                                        (not (eq (first argument-spec) :variable)))
                                (:fail)))
                            (<<- arguments (defined-variable environment)))
                       (and (<- (delimiter char index)
                                (:transform (seq)
                                  (when (null argument-spec)
                                    (:fail))
                                  (a:if-let ((delimiter (getf (bp:node-initargs* (first argument-spec)) :delimiter)))
                                    (list delimiter (aref delimiter 0) 0)
                                    (:fail))))
                            (<<- arguments (:transform
                                               (seq (* (<<- characters (and (not char) :any)))
                                                    (+ (:transform character
                                                         (if (and (< index (length delimiter))
                                                                  (char= character (aref delimiter index)))
                                                             (incf index)
                                                             (:fail))))
                                                    (:transform (seq) (unless (= index (length delimiter)) (:fail)))
                                                    #+no (+ (and (:transform c (print (list :@ c)))
                                                                 (<- (char index)
                                                                     (:transform (seq)
                                                                                 (print (list (aref delimiter index) (1+ index)))
                                                                                 (if (= index (length delimiter))
                                                                                     (:fail)
                                                                                     (list (aref delimiter index) (1+ index)))))
                                                                 char)))
                                             (prog1
                                                 (bp:node* (:chunk :content (coerce (nreverse characters) 'string)))
                                               (setf characters '())))))
                       (and (:transform (seq)
                              (when (or (null argument-spec)
                                        (not (null (getf (bp:node-initargs* (first argument-spec)) :delimiter))))
                                (:fail)))
                            (<<- arguments (element environment))))
                   (:transform (seq) (pop argument-spec))))
           (:transform (seq)
             (unless (or (null argument-spec)
                         (equal name "eatcr") (equal name "ttref") (equal name "clref") (equal name "EatPunc")
                         (equal name "dobegincom") (equal name "doobegincom") (equal name "doformat"))
               (break "~A ~A ~A" name arguments argument-spec)
               (let ((mode           (env:lookup :mode :traversal environment
                                                 :if-does-not-exist :normal))
                     (supplied-count (length arguments)))
                 (:fatal (format nil "Too few arguments to ~(~A~) macro ~A. ~D required, but only ~D supplied."
                                 mode name (+ supplied-count (length argument-spec)) supplied-count)))))))
  (bp:node* (:other-command-application :name   name
                                        :bounds (cons start end))
    (* (:argument . *) (nreverse arguments))))

(when nil
  (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
    (setf (env:lookup "VSkip" :macro env) '(((:ARGUMENT NIL :LEVEL 1 :NUMBER 1 :DELIMITER " plus ")
                                             (:ARGUMENT NIL :LEVEL 1 :NUMBER 2 :DELIMITER "!"))))
    (bp:with-builder ('list)
      (parser.packrat:parse
       `(document "somefile" 0 ,env)
       "\\VSkip1 plus 2!"))))

(when nil
 (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
   (bp:with-builder ('list)
     (parser.packrat:parse
      `(document "somefile" 0 ,env)
      "\\def\\barbar#1#2{foo#1bar#2baz}
\\barbar 1{2}3"))))

(when nil
  (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
    (setf (env:lookup "Vskip" :macro env) '(((:ARGUMENT NIL :LEVEL 1 :NUMBER 1 :DELIMITER "!"))))
    (bp:with-builder ('list)
      (parser.packrat:parse
       `(document "somefile" 0 ,env)
       "{\\Vskip1pc!}"))))

(when nil
 (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
   (register-builtin-macros env)
   (setf (env:lookup "pageno" :variable env) t)
   (bp:with-builder ('list)
     (parser.packrat:parse
      `(document "somefile" 0 ,env)
      "\\global\\advance\\pageno  -1"))))

(when nil
 (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
   (register-builtin-macros env)
   (setf (env:lookup "toc" :if env) t)
   (setf (env:lookup "tocfile" :macro env) 0)
   (setf (env:lookup "tocfalse" :macro env) 0)
   (bp:with-builder ('list)
     (parser.packrat:parse
      `(document "somefile" 0 ,env)
      "\\iftoc\\immediate\\closeout\\tocfile\\global\\tocfalse\\fi"))))

(when nil
 (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
   (register-builtin-macros env)
   (setf (env:lookup "toc" :if env) t)
   (setf (env:lookup "tocfile" :macro env) 0)
   (setf (env:lookup "tocfalse" :macro env) 0)
   (bp:with-builder ('list)
     (parser.packrat:parse
      `(document "somefile" 0 ,env)
      "\\mathchardef \\spBS \"086E %
\\chardef\\bslash=123 %
\\def\\BSlash{\\ifmmode\\spBS\\else\\iftt\\bslash\\else{$\\spBS$}\\fi\\fi}"))))

(when nil
 (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
   (register-builtin-macros env)
   (setf (env:lookup "tt" :if env) t)
   (setf (env:lookup "spVB" :macro env) 0)
   (bp:with-builder ('list)
     (parser.packrat:parse
      `(document "somefile" 0 ,env)
      "\\def\\VB{\\ifmmode\\spVB\\else\\iftt|\\else{$\\vert$}\\fi\\fi}"))))

(defrule newif (environment)
    (bounds (start end)
      (seq "\\newif"
           (skippable*) (<- name1 (name))
           (? (seq (? (whitespace/in-line+)) (<- name2 (name))))))
  ;; Side effect: define "if", "true" and "false" macros.
  (with-name-string (name name1)
    (let* ((stem  (subseq name 2))
           (true  (concatenate 'string stem "true"))
           (false (concatenate 'string stem "false")))
      (setf (lookup stem  :if    environment) t
            (lookup true  :macro environment) 0
            (lookup false :macro environment) 0)))
  ;; Result
  (bp:node* (:newif :bounds (cons start end))
    (1    (:name  . 1) name1)
    (bp:? (:name2 . 1) name2)))

(defrule fi ()
  (seq "\\fi" (and (not (identifier)) (seq))))

(defrule else ()
  (seq "\\else" (and (not (identifier)) (seq))))

(defrule or* ()
  (seq "\\or" (and (not (identifier)) (seq))))

(defrule if-case (environment)
  (seq/ws "\\ifcase" (<- number (or (defined-variable environment)
                                    (element environment)))
          (* (and (not (or (or*) (else) (fi)))
                  (<<- cases (element environment))))
          (* (seq/ws (or*) (* (and (not (or (or*) (else) (fi)))
                                   (<<- cases (element environment))))))
          (? (seq/ws (else) (* (and (not (fi))
                                    (<<- cases (element environment))))))
          (fi))
  (bp:node* (:if-case)
    (1 (:number . 1) number)
    (* (:case   . *) cases)))

(when nil
  (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
    (register-builtin-macros env)
    (setf (env:lookup "figtype" :variable env) t)
    (setf (env:lookup "finishfig" :macro env) 0)
    (setf (env:lookup "finishrulefig" :macro env) 0)
    (setf (env:lookup "finishboxfig" :macro env) 0)
    (bp:with-builder ('list)
      (parser.packrat:parse
       `(if-case  ,env)
       "\\ifcase\\figtype\\relax\\finishfig\\or\\finishrulefig\\or\\finishboxfig\\fi"))))


(defrule user-if (environment)
  (seq/ws (seq "\\if" (or (seq/ws "odd" (or (defined-variable environment)
                                            (element environment)))
                          (seq/ws "num" (or (defined-variable environment)
                                            (element environment))
                                        (or #\< #\= #\>)
                                        (or (defined-variable environment)
                                            (element environment)))
                          (seq/ws "x" (element environment) (element environment))
                          (:transform (<- name (identifier))
                            (unless (env:lookup name :if environment
                                                     :if-does-not-exist nil)
                              (:fail)))

                          (seq/ws (element environment) (element environment))))
          (* (seq (skippable*)
                  (and (not (or "\\fi" "\\else"))
                       (<<- consequent (element environment)))))
          (? (seq/ws "\\else" (* (seq (skippable*)
                                      (and (not "\\fi")
                                           (<<- alternative (element environment)))))))
          "\\fi")
  (bp:node* (:if :test name)
    ; (1 (:test        . 1) name)
    (* (:consequent  . *) (nreverse consequent))
    (* (:alternative . *) (nreverse alternative))))

(when nil
  (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
    (register-builtin-macros env)
    (setf (env:lookup "lr" :macro env) 0)
    (setf (env:lookup "lr" :macro env) 0)
    (setf (env:lookup "leftcolumn" :variable env) t)
    (setf (env:lookup "columnbox" :variable env) t)
    (bp:with-builder ('list)
      (parser.packrat:parse
       `(document "somefile" 0 ,env)
       "\\if L\\lr
\\global\\setbox\\leftcolumn=\\columnbox \\global\\let\\lr=R
\\else
\\doubleformat
\\global\\let\\lr=L\\fi"))))

(when nil
 (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
   (register-builtin-macros env)
   (setf (env:lookup "lr" :macro env) 0)
   (setf (env:lookup "lr" :macro env) 0)
   (setf (env:lookup "leftcolumn" :variable env) t)
   (setf (env:lookup "columnbox" :variable env) t)
   (bp:with-builder ('list)
     (parser.packrat:parse
      `(document "foo" 0 ,env)
      "\\newif \\ifnullabel  \\nullabelfalse % might be overridden in setup-options

     \\def\\None{None.}

     \\def\\EatPunc #1{} % Takes care of eating a trailing \".\" or \"!\"

     \\def\\label #1:#2{\\ifx#2:\\truelabel{#1}\\else\\labelNone{#1}\\fi}"))))

(when nil
 (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
   (register-builtin-macros env)
   (setf (env:lookup "smallab" :variable env) t)
   (setf (env:lookup "prbseven" :macro env) 0)
   (setf (env:lookup "prbtwelve" :macro env) 0)
   (setf (env:lookup "ignorepar" :macro env) 0)
   (bp:with-builder ('list)
     (parser.packrat:parse
      `(document "foo" 0 ,env)
      "\\ifsmallab
\\noindent\\hskip -4pc \\hbox to 4pc{{\\prbseven #1 }\\hss}\\expandafter\\ignorepar
\\else
\\hbox{\\prbtwelve #1:}
\\penalty20000
\\vskip 2pt plus 2pt
\\penalty20000
\\fi
"))))


(defrule newskip/dimen (environment)
    (bounds (start end)
      (seq/ws (seq "\\new" (or "skip" "dimen")) (<- name (name))))
  ;; Side effect: define variable
  (with-name-string (name name)
    (setf (lookup name :variable environment) t))
  ;; Result
  (bp:node* (:newskip :bounds (cons start end))
    (1 (:name . 1) name)))

(when nil
  (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
    (setf (env:lookup :global? :traversal env) t)
    (register-builtin-macros env)
    (setf (env:lookup "smallab" :variable env) t)
    (setf (env:lookup "prbseven" :macro env) 0)
    (setf (env:lookup "prbtwelve" :macro env) 0)
    (setf (env:lookup "ignorepar" :macro env) 0)
    (bp:with-builder ('list)
      (parser.packrat:parse
       `(document "foo" 0 ,env)
       "\\newdimen \\changedepth
\\changedepth=0.15\\baselineskip"))))

#+no (defrule skip (environment) ; TODO why treat this specially?
    (seq/ws "\\parskip" (<- name (name)))
  )

(defrule countdef (environment)
    (seq #\\ (or (seq/ws "countdef" (<- name (name))
                         #\= (+ (<<- number (guard digit-char-p))))
                 (seq/ws "newcount" (<- name (name)))))
  ;; Side effect: define variable
  (let ((name (getf (bp:node-initargs* name) :content)))
    (check-type name string)
    (setf (lookup name :variable environment) t))  ; TODO to consume = token and value token
  ;; Variable
  (bp:node* (:counter-definition)
    (1    (:name . 1)   name)
    (bp:? (:number . 1) number)))

(when nil
 (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
   (bp:with-builder ('list)
     (parser.packrat:parse
      `(document "foo" 0 ,env)
      "\\countdef\\pageno=0
\\pageno=0"))))

(when nil
 (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
   (register-builtin-macros env)
   (setf (env:lookup "incom" :if env) t)
   (bp:with-builder ('list)
     (parser.packrat:parse
      `(document "foo" 0 ,env)
      "\\ifincom\\else\\global\\setbox\\comline\\null\\fi"))))

(defrule new (environment)
    (bounds (start end)
      (seq/ws (seq "\\new" (or "toks" "box" "write" "insert")) (<- name (name))))
  ;; Side effect: define variable and macro
  (with-name-string (name name)
    (setf (lookup name :variable environment) t)
    (setf (lookup name :macro environment) 0))
  (bp:node* (:new :bounds (cons start end))
    (1 (:name . 1) name)))

(defrule openout (environment)
    (bounds (start end)
      (seq/ws "\\openout" (<- name1 (name)) (<- name2 (name))))
  (bp:node* (:openout :bounds (cons start end))
    (1 (:name1 . 1) name1)
    (1 (:name2 . 1) name2)))

(defrule advance (environment)
    (bounds (start end)
      (seq/ws "\\advance" (<- name (or (register-reference environment)
                                       (defined-variable environment)))
              (? "by") (<- amount (or (numeric-expression environment)
                                      (chunk))))) ; TODO numeric-expression
  (bp:node* (:advance :bounds (cons start end))
    (1 (:variable . 1) name)
    (1 (:amount   . 1) amount)))

(when nil
  (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
    (register-builtin-macros env)
    (setf (env:lookup "iskip" :variable env) t
          (env:lookup "iiiskip" :variable env) t)
    (bp:with-builder ('list)
      (parser.packrat:parse
       `(document "foo" 0 ,env)
       "\\iiiskip=\\leftskip	\\advance\\iiiskip 1.5pc\\iskip  =\\iiiskip
\\advance\\hsize by -\\leftskip
\\advance\\hsize by -2\\leftskip
\\advance\\hsize -2pt
\\advance\\dimen1 by -\\ht0"))))

(when nil
  (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
    (setf (env:lookup "iiiskip" :variable env) t
          (env:lookup "leftskip" :variable env) t)
    (bp:with-builder ('list)
      (parser.packrat:parse
       `(user-assignment ,env)
       "\\iiiskip=\\leftskip"))))

(when nil
 (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
   (bp:with-builder ('list)
     (parser.packrat:parse
      `(newskip/dimen ,env)
      "\\newskip \\foo \\bar = 1pc"))))

(defrule ifdim (environment)
  (seq/ws (or (seq/ws "\\ifdim" (<- test (element environment)))
              "\\ifvmode")
            (* (<<- consequent (and (not (or "\\fi" "\\else")) ; TODO whitespace
                                    (element environment))))
            (? (seq/ws "\\else" (* (<<- alternative (and (not "\\fi")
                                                         (element environment))))))
            "\\fi")
  nil)

(defrule defined-variable (environment)
    (<- name (name))
  (with-name-string (name name)
    (unless (env:lookup name :variable environment
                             :if-does-not-exist nil)
      (:fail)))
  name)

(defrule user-assignment (environment)
    (seq/ws (? (<- global? (:transform "\\global" t)))
            (<- variable (defined-variable environment))
            (or (seq/ws #\= (<- value (or (numeric-expression environment)
                                          (element environment))))
                (<- value (or (numeric-expression environment)
                              ; (defined-variable environment)
                              (user-macro-application environment)
                              (hbox environment)
                              (chunk)))))
  (bp:node* (:assignment :global? global?)
    (1 (:variable . 1) variable)
    (1 (:value    . 1) value)))

(defrule user-reference (environment)
    (bounds (start end)
      (seq/ws "\\the" (<- name (defined-variable environment))))
  (bp:node* (:register-read :bounds (cons start end))
    (1 (:name . 1) name)))

(defrule %numeric-expression (environment)
  (or (:transform (seq #\- (<- factor (%numeric-expression environment)))
        (bp:node* (:multiplication :factor -1)
          (1 :factor factor)))
      (:transform
          (seq (or (seq #\. (+ (<<- digits (guard digit-char-p))))
                   (seq (+ (<<- digits (guard digit-char-p)))
                        (? (seq #\. (+ (<<- digits (guard digit-char-p)))))))
               (<- factor (%numeric-expression environment)))
        (bp:node* (:multiplication :factor (nreverse digits))
          (1 :factor factor)))
      (defined-variable environment)
      (register-reference environment)))

(defrule numeric-expression (environment)
  (or (seq/ws (%numeric-expression environment) "plus" (numeric-expression environment))
      ; (seq/ws (%numeric-expression environment) "minus" (numeric-expression environment))
      (%numeric-expression environment)))

(defrule register-number (environment)
  (+ (guard digit-char-p)))

(defrule register-reference* (environment)
  (or (register-number environment)
      (defined-variable environment)))

(defrule register-assignment (environment)
  (seq/ws (seq "\\" (or "count" "skip")) (<- name (name)) #\= (<- value (chunk))))

(defrule register-reference (environment)
  (seq/ws (seq "\\" (or "ht" "dp" "box" "dimen")) (register-number environment)))

(defrule register-copy (environment)
  (seq/ws "\\copy" (register-reference* environment)))

(defrule box-assignment (environment)
    (seq/ws "\\setbox" (<- register (register-reference* environment))
            (? #\=) (<- value (or (defined-variable environment)
                                  (vsplit environment)
                                  (element environment))))
  (bp:node* (:setbox)
    (1 (:register . 1) register)
    (1 (:value    . 1) value)))

(defrule box-reference (environment)
  (seq/ws "\\" (or "unvbox" "unhbox" "box") (register-reference* environment)))

(defrule hbox (environment)
    (bounds (start end)
      (seq/ws "\\" (<- kind (or (:transform "hbox" :hbox)
                                (:transform "vbox" :vbox)
                                (:transform "vtop" :vtop)))
              (or (seq/ws "to" (<- to (name)))
                  (seq/ws "spread" (<- spread (name)))
                  (seq))
              (or (seq/ws #\{ (* (<<- elements (element environment))) #\})
                  (:transform (seq) (bp:node* (:pending-arguments))))))
  (bp:node* (kind :bounds (cons start end))
    (bp:? (:to      . 1) to)
    (bp:? (:spread  . 1) spread)
    (*    (:element . *) (nreverse elements))))

(when nil
  (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
    (register-builtin-macros env)
    (setf (env:lookup "fullhsize" :variable env) t)
    (setf (env:lookup "qquad" :macro env) 0)
    (bp:with-builder ('list)
      (parser.packrat:parse
       `(document "foo" 1 ,env)
       "\\def\\deffigrefs#1{\\def\\figref##1{{\\def##1{\\message{Figure ``\\string##1'' is not defined.}%
Figure $nn$--$mm$ (\\string##1)}#1##1}}}
"))))

(defrule vsplit (environment)
    (bounds (start end)
      (seq/ws "\\vsplit" (<- box (register-reference* environment))
              "to" (<- dimension (or (defined-variable environment)
                                     (element environment)))))
  (bp:node* (:vsplit :bounds (cons start end))
    (1 (:box . 1)       box)
    (1 (:dimension . 1) dimension)))

(defrule hrule (environment)
  (bounds (start end)
   (seq "\\hrule" (* (seq (skippable*) (or (seq/ws "height"
                                                (<- height (or (defined-variable environment)
                                                               (:transform
                                                                (bounds (start2 end2)
                                                                  (+ (<<- characters (and (not (or (skippable) #\} #\\ "depth" "width")) :any))))
                                                                (prog1
                                                                    (bp:node* (:chunk :content (coerce (nreverse characters)'string)
                                                                                      :bounds  (cons start2 end2)))
                                                                  (setf characters '()))))))
                                           (seq/ws "depth"
                                                   (<- depth (or (defined-variable environment)
                                                                  (:transform
                                                                   (bounds (start2 end2)
                                                                     (+ (<<- characters (and (not (or (skippable) #\} #\\ "height" "width")) :any))))
                                                                   (prog1
                                                                       (bp:node* (:chunk :content (coerce (nreverse characters) 'string)
                                                                                         :bounds  (cons start2 end2)))
                                                                     (setf characters '()))))))
                                           (seq/ws "width"
                                                   (<- width (or (defined-variable environment)
                                                                 (:transform
                                                                     (bounds (start2 end2)
                                                                       (+ (<<- characters (and (not (or (skippable) #\} #\\ "height" "depth")) :any))))
                                                                   (prog1
                                                                       (bp:node* (:chunk :content (coerce (nreverse characters) 'string)
                                                                                         :bounds  (cons start2 end2)))
                                                                     (setf characters '())))))))))))
  (bp:node* (:hrule :bounds (cons start end))
    (bp:? (:height . bp:?) height)
    (bp:? (:depth  . bp:?) depth)
    (bp:? (:width  . bp:?) width)))

(when nil
  (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
    (register-builtin-macros env)
    (setf (env:lookup :current-file :traversal env) #P"~/code/cl/common-lisp/dpans/dpANS3/appendix-removed.tex")
    (setf (env:lookup "fullhsize" :variable env) t)
    (setf (env:lookup "qquad" :macro env) 0)
    (bp:with-builder ('list)
      (parser.packrat:parse
       `(hrule ,env)
       "\\hrule height1.5pt width \\fullhsize"))))

(defrule string* (environment)
  (seq/ws "\\string" (<- name (name)))
  (bp:node* (:name)
    (1 (:name . 1) name)))

(defrule expandafter (environment)
  (seq/ws "\\expandafter" (name)))

(defrule romannumeral (environment)
    (bounds (start end)
      (seq/ws "\\romannumeral" (<- number (numeric-expression environment))))
  (bp:node* (:romannumeral :bounds (cons start end))
    (1 (:number . 1) number)))

(when nil
 (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
   (register-builtin-macros env)
   (setf (env:lookup "fullhsize" :variable env) t)
   (bp:with-builder ('list)
     (parser.packrat:parse
      `(document "foo" 1 ,env)
      "\\hrule height1.5pt width \\fullhsize"))))

(when nil
 (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
   (register-builtin-macros env)
   (bp:with-builder ('list)
     (parser.packrat:parse
      `(document "foo" 0 ,env)
      "\\def\\shortpar{\\begingroup\\def\\par{\\endgraf\\endgroup\\par}
\\advance\\rightskip}"))))

(when nil
  (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
    (register-builtin-macros env)
    (bp:with-builder ('list)
      (parser.packrat:parse
       `(ifdim ,env)
       "\\ifdim \\lastskip<#1 \\ifdim \\lastskip>0pc \\removelastskip\\fi \\vskip#1\\NIPS\\fi}"))))

(when nil
 (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
   (register-builtin-macros env)
   (bp:with-builder ('list)
     (parser.packrat:parse
      `(user-macro-application ,env)
      "\\parskip 0pt"))))

(defrule font (environment)
    (bounds (start end)
      (seq/ws (or "\\font" "\\Font") (<- name (name)) ; TODO capitalized is temp
              #\= (* (<<- file (guard alphanumericp)))
              (? (seq/ws "\\sc" (<- name2 (name))))))
  ;; Side effect: define macro
  (with-name-string (name name)
    (let ((file (coerce (nreverse file) 'string)))
      (setf (lookup name :macro environment) 0)))
  ;; Result
  (bp:node* (:font :bounds (cons start end))
    (1 (:name . 1) name)))

(when nil
 (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
   (register-builtin-macros env)
   (bp:with-builder ('list)
     (parser.packrat:parse
      `(document "foo" 0 ,env)
      "\\font\\mifive	= cmmi5"))))


(when nil
  (bp:with-builder ('list)
   (parser.packrat:parse '(font 'nil) "\\Font\\prbeleven = cmbx10 \\sc \\mh")))

(defrule element (environment)
  (and ;; Do not allow unpaired \\end* unless within a macro
       (or (:transform (seq)
             (unless (env:lookup :definition :traversal environment
                                             :if-does-not-exist nil)
               (:fail)))
           (not (seq "\\" (or "endissue"
                              "endlist"
                              (seq "end" (* "sub") "section")
                              ))))
       (or ;; Lexical stuff
           (comment)
           (spacing-command)
           (verb)
           (indexed-char)

           ;; TeX stuff
           (catcode environment) (chardef environment) (mathchardef environment)
           (newskip/dimen environment) (ifdim environment)
           (new environment)
                                        ; (skip environment)
           (countdef environment)
           (advance environment)

           (if-case environment)
           (newif environment) (user-if environment)

           (register-assignment environment) (register-reference environment) (register-copy environment)
           (box-assignment environment)      (box-reference environment)
           (user-assignment environment)     (user-reference environment)

           (string* environment)
           (expandafter environment)

           (romannumeral environment)

           (hbox environment)
           (hrule environment)
           (font environment)

           ;; TeX table stuff
           (halign environment)
           (settabs environment)
           (column-separator) ; TODO only in table or command definition
           (span)
           (row-terminator)

           (openout environment)

           ;; Markup
           #+no (b environment) (bf environment) #+no (bold environment)
                                                 #+no (i environment) #+no (ital environment) (it environment)
           (f environment)
           (tt environment)
           (rm environment)

           (input environment)
           (include-dictionary environment)

           ;; Semantic
           (head environment)
           (head1 environment)
           (chapter environment)
           (section environment)
           (simple-section environment)
           (define-section)
           (define-figure)

           (item-list environment)         (list-item environment)
           (enumeration-list environment)  (enumeration-item environment)
           (definition-list environment)   (definition-item environment)

           (code)

           (issue-annotation environment)
           (editor-note)
           (reviewer)

           (component-label environment)
           (none environment)
           (component environment)

           (term environment)
           (newterm environment)
           (newtermidx environment)
           (ftype environment)

           (reference environment)

           (index environment)

           (lambda-list-keyword)
           (call-syntax environment)
           (param environment)
           (kwd environment)
           (row-terminator* environment)
           (bnf-rule environment)

           (dpans-table environment)

           ;; Glossary
           (glossary-list environment) (gentry environment)

           (def environment)
           (argument)                   ; TODO
           (let-macro environment)
           (global-modifier environment) ; TODO should not be needed
           (user-macro-application environment)

           (block* environment)
           (bracket-group environment)
           (math-display environment)   ; must precede `math-group'
           (math-group environment)

           (tilde environment)
           (mdash)
           (paragraph-break)
           (chunk)

           (skippable+))))

(defrule document (filename include-depth environment) ; TODO file
    (bounds (start end) (* (<<- elements (element environment))))
  (bp:node* (:file :filename      filename
                   :include-depth (or include-depth 0)
                   :bounds        (cons start end))
    (* :element (nreverse elements))))
