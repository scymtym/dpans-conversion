(cl:in-package #:dpans-conversion.parser)

(defgrammar dpans
  (:cached? nil)
  (:class parser.packrat.grammar.string:simple-string-grammar))

(in-grammar dpans)

;;; Lexical stuff

(defrule end-of-line ()
  (or #\Newline (not :any)))

(defrule whitespace/in-line+ ()
  (+ (or #\Space #\Tab)))

(defrule comment-line ()
    (bounds (start end)
      (seq #\% (* (<<- content (and (not (end-of-line)) :any))) (end-of-line)))
  (let ((content (coerce (nreverse content) 'string)))
    (bp:node* (:comment-line :content content :source (cons start end)))))

(defrule comment ()
    (bounds (start end)
      (seq (<<- lines (comment-line))
           (* (seq (? (whitespace/in-line+)) (<<- lines (comment-line))))))
  (bp:node* (:comment :source (cons start end))
    (* (:line . *) (nreverse lines))))

(defrule skippable ()
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
    (seq (<<- content (and (not (or #\{ #\} #\\ #\. #\, #\; #\# #\& #\$ #\= #\_
                                    (terminator)))
                           :any))
         (* (<<- content (and (not (or #\{ #\} #\\ #\. #\, #\; #\# #\& #\$ #\= #\_
                                       (guard digit-char-p)
                                       (terminator)))
                              :any))))
  (coerce (nreverse content) 'string))

(defrule identifier-with-dot ()
    (seq (<<- content (and (not (or #\{ #\} #\\ #\# #\& #\$ #\= #\_
                                    (terminator)))
                           :any))
         (* (<<- content (and (not (or #\{ #\} #\\ #\# #\& #\$ #\= #\_
                                       (guard digit-char-p)
                                       (terminator)))
                              :any))))
  (coerce (nreverse content) 'string))

(defrule name ()
    (bounds (start end) (seq "\\" (<- name (identifier))))
  (bp:node* (:name :name name :bounds (cons start end))))

(defrule spacing-command ()
    (seq #\\ (or #\, #\: #\> #\; ; spaces
                 #\/))           ; italics correction
  nil)

(defrule non-breaking-space ()
    (bounds (start end) #\~)
  (bp:node* (:non-breaking-space :source (cons start end))))

(defrule escaped-character ()
    (seq #\\ (<- character (or #\\ #\@ #\= #\Space #\' #\[ #\]
                               #\Newline ; not sure about newline
                               #\% #\# #\_ #\{ #\} #\& #\$ #\.)))
  character)

(defrule indexed-char ()
    (seq "\\char" (skippable*) (? #\') (+ (<<- id (guard digit-char-p))))
  (let ((code (parse-integer (coerce (nreverse id) 'string) :radix 8)))
    (bp:node* (:word :content (string (code-char code))))))

(defrule word ()
    (+ (<<- characters (or (escaped-character)
                           (and (not (or #\{ #\} #\\ #\% #\& #\$ #\~ #\# ; #\.
                                         (paragraph-break))) ; TODO make non-result version
                                :any))))
  (bp:node* (:word :content (coerce (nreverse (remove nil characters)) 'string))))

(defrule paragraph-break ()
  (bounds (start end) (seq #\Newline (* (or #\Space #\Tab)) #\Newline))
  (bp:node* (:paragraph-break :source (cons start end))))

(defrule verb ()
    (seq "\\verb" delimiter
         (* (<<- content (and (not delimiter) :any)))
         delimiter)
  (let ((content (coerce (nreverse content) 'string)))
    (bp:node* (:verbatim :content content))))

(define-command (b :kind :bold)
  (1* :element (element)))

(defrule bf ()
    (bounds (start end)
      (seq "\\bf" (* (<<- elements (and (not (or #\} #\&)) (element))))))
  (bp:node* (:bold :source (cons start end))
    (* :element (nreverse elements))))

(define-command (bold :kind :bold)
  (1* :element (element)))

(define-command (i :kind :italic)
  (1* :element (element)))

(define-command (ital :kind :italic)
  (1* :element (element)))

(defrule it ()
    (bounds (start end)
      (seq "{\\it" (* (<<- elements (and (not #\}) (element)))) #\}))
  (bp:node* (:italic :source (cons start end))
    (* :element (nreverse elements))))

(define-command f ; "fixed", that is monospace font
  (1* :element (element)))

(defrule tt ()
    (bounds (start end)
      (seq "{\\tt" (* (<<- elements (and (not #\}) (element)))) #\}))
  (bp:node* (:typewriter :source (cons start end))
    (* :element (nreverse elements))))

(defrule rm ()
  (bounds (start end)
          (seq "{\\rm" (* (<<- elements (and (not #\}) (element)))) #\}))
  (bp:node* (:roman :source (cons start end))
    (* :element (nreverse elements))))

(define-command hbox
  (1* :element (element)))

(defmacro define-group (name kind open-delimiter close-delimiter)
  `(defrule ,name ()
       (bounds (start end)
         (seq ,open-delimiter (skippable*)
              (* (<<- elements (and (not ,close-delimiter) (element))))
              (skippable*) ,close-delimiter)
              (:transform (seq) nil)) ; HACK
     (bp:node* (,kind :source (cons start end))
       (* (:element . *) (nreverse elements)))))

(define-group block*        :block         #\{  #\})
(define-group bracket-group :bracket-group #\[  #\])
(define-group math-group    :math          #\$  #\$)
(define-group math-display  :math-display  "$$" "$$")

(defrule text ()
    (+ (or (seq #\{
                (<<- blocks (:transform (seq)
                              (prog1
                                  (when block
                                    (list :block (coerce (nreverse block) 'string)))
                                (setf block '()))))
                (? (<<- blocks (element)))
                #\})
           (<<- block (and (not (or #\{ #\} #\\ #\%)) :any))))
  (let ((last-block (when block
                      (list :block (coerce (nreverse block) 'string)))))
    (nreverse (remove nil (list* last-block blocks)))))

(defrule code ()
    (bounds (start end)
      (seq "\\code" #\Newline
           (* (<<- content (and (not "\\endcode") :any)))
           "\\endcode"))
  (let ((content (coerce (nreverse content) 'string)))
    (bp:node* (:code :content content :bounds (cons start end)))))

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
          `(defrule ,name ()
               (bounds (start end)
                (seq ,keyword
                     ,@(map 'list #'make-argument-expression arguments)
                     (:transform (seq) nil) ; HACK
                     ))
             (bp:node* (,kind :source (cons start end))
               ,@(map 'list #'make-argument-result arguments variables))))))))

(defrule input ()
    (bounds (start end)
      (seq "\\input"
           (skippable+)
           (* (<<- name (and (not (skippable)) :any)))
           (terminator)))
  (let ((name (coerce (nreverse name) 'string)))
    (bp:node* (:input :name name :source (cons start end)))))

(define-command term
  (1 :name (word)))

(define-command newterm
  (1 :name (word)))

(define-command newtermidx
  (1 :name (word))
  (1 :term (word)))

(define-command ftype
  (1 :name (element)))

;; This would not be needed if we could handle expansions better.
(defrule seesec ()
    (bounds (start end)
      (seq "\\" (<- which (or #\s #\S)) "ee" (or "section" "chapter") "\\"
           (<- name (identifier))))
  (bp:node* (:block :source (cons start end))
    (* :element (list (bp:node* (:word :content (concatenate 'string (string which) "ee ")))
                      (bp:node* (:secref :source (cons start end))
                        (1 (:name . 1) (bp:node* (:word :content name))))))))

(defrule seefig ()
    (bounds (start end)
      (seq "\\" (<- which (or #\s #\S)) "ee" "figure" "\\"
           (<- name (identifier))))
  (bp:node* (:block :source (cons start end))
    (* :element (list (bp:node* (:word :content (concatenate 'string (string which) "ee ")))
                      (bp:node* (:figref :source (cons start end))
                        (1 (:name . 1) (bp:node* (:word :content name))))))))

(defrule secref ()
  (bounds (start end)
          (seq "\\secref" (or (seq #\\ (<- name (identifier)))
                              (<- name (argument)))))
  (bp:node* (:secref :source (cons start end))
    (1 (:name . 1) (if (stringp name)
                       (bp:node* (:word :content name)) ; TODO word is a hack
                       name))))

(defrule chapref ()
  (bounds (start end)
    (seq "\\chapref" (or (seq #\\ (<- name (identifier)))
                         (<- name (argument)))))
  (bp:node* (:secref :source (cons start end))
    (1 (:name . 1) (if (stringp name)
                       (bp:node* (:word :content name))
                       name))))

(define-command keyref ; lambda list keyword reference
  (1 :name (element)))

(define-command typeref
  (1 :name (element)))

(define-command declref
  (1 :name (element)))

(define-command specref
  (1 :name (element)))

(define-command funref
  (1 :name (element)))

(define-command macref
  (1 :name (element)))

(define-command varref
  (1 :name (element)))

(define-command conref ; constant
  (1 :name (element)))

(defrule figref ()
    (bounds (start end)
      (seq "\\" (or #\f #\F) "igref"
           (skippable*) (or (seq #\{ (<- name (element)) (skippable*) #\})
                            (seq #\\ (<- name (identifier))))))
  (bp:node* (:figref :source (cons start end))
    (1 (:name . 1) (if (stringp name)
                       (bp:node* (:word :content name))
                       name))))

(defrule miscref ()
  (bounds (start end)
    (seq "\\misc" (? "ref")
         (skippable*) #\{ (<- name (element)) (skippable*) #\}))
  (bp:node* (:miscref :source (cons start end))
    (1 (:name . 1) name)))

(defrule reference ()
  (or (keyref)
      (typeref)
      (declref)
      (specref)
      (funref)
      (macref)
      (varref)
      (conref)
      (figref)
      (miscref)))

(macrolet
    ((define ()
       (let ((rules '()))
         (flet ((one-index (keyword
                            &key (rule-name (a:symbolicate '#:index- keyword))
                                 (command   (string-downcase keyword)))
                  (let ((string (concatenate 'string "\\idx" command))
                        (which  (a:make-keyword keyword)))
                    (push rule-name rules)
                    `(defrule ,rule-name ()
                       (bounds (start end)
                         (seq ,string
                              (skippable*) #\{ (<- name (element)) (skippable*) #\}))
                       (bp:node* (:index :which  ,which :bounds (cons start end))
                         (1 (:name . 1) name))))))
           `(progn
              ,@(map 'list #'one-index '(#:ref #:keyref #:code #:kwd
                                         #:text #:term #:example #:packref))
              (defrule index ()
                (or ,@(map 'list #'list rules))))))))
  (define))

;;;

(macrolet
    ((define ()
       (let ((rules '()))
         (flet ((one-keyword (keyword
                              &key (rule-name (a:symbolicate '#:lambda-list-keyword- keyword))
                                   (command   (remove #\- (string-downcase keyword))))
                  (let ((string (concatenate 'string "\\" command))
                        (which  (a:make-keyword keyword)))
                    (push rule-name rules)
                    `(defrule ,rule-name ()
                       (bounds (start end)
                               (seq ,string (and (not (identifier)) (seq)) (:transform (seq) nil))) ; HACK
                       (bp:node* (:lambda-list-keyword :which  ,which
                                                       :bounds (cons start end)))))))
           `(progn
              ,@(map 'list #'one-keyword '(#:allow-other-keys
                                           #:aux #:body #:environment #:key
                                           #:optional #:rest #:whole))
              ,(one-keyword '#:optional :rule-name 'lambda-list-keyword-opt
                                        :command   "opt")
              (defrule lambda-list-keyword ()
                (or ,@(map 'list #'list rules))))))))
  (define))

(defrule definition-special-operator ()
    (bounds (start end)
      (seq "\\" "Defspec" (? (or (seq "WithValues" (? "Newline")) "NoReturn"))
                  (seq (skippable*) (? #\{) (<-  name          (element))     (skippable*) (? #\}))
                  (seq (skippable*) #\{     (* (<<- arguments     (element))) (skippable*) #\})
               (? (seq (skippable*) #\{     (* (<<- return-values (element))) (skippable*) #\}))))
  (bp:node* (:special-operator-definition :source (cons start end))
    (1 (:name . 1)         name)
    (* (:argument . *)     arguments)
    (* (:return-value . *) return-values)))

(defrule definition-defun ()
    (bounds (start end)
      (seq "\\" "Defun" (? (or (seq "WithValues" (? "Newline")) "NoReturn"))
              (seq (skippable*) (? #\{) (<-  name          (element))     (skippable*) (? #\}))
              (seq (skippable*) #\{     (* (<<- arguments     (element))) (skippable*) #\})
           (? (seq (skippable*) #\{     (* (<<- return-values (element))) (skippable*) #\}))))
  (bp:node* (:function-definition :source (cons start end))
    (1 (:name . *)         name)
    (* (:argument . *)     (nreverse arguments))
    (* (:return-value . *) (nreverse return-values))))

(defrule definition-defun/multi ()
    (bounds (start end)
      (seq "\\DefunMultiWithValues"
           (skippable*) #\{ (* (<<- arguments     (element)))            (skippable*) #\}
           (skippable*) #\{ (* (<<- return-values (element)))            (skippable*) #\}
           (skippable*) #\{ (* (seq "\\entry{" (<<- names (element)) #\} (skippable*))) #\}))
  (bp:node* (:function-definition :source (cons start end))
    (* (:name         . *) (nreverse names))
    (* (:argument     . *) (nreverse arguments))
    (* (:return-value . *) (nreverse return-values))))

(defrule definition-defmacro ()
    (bounds (start end)
      (seq "\\" "Defmac" (? (or (seq "WithValues" (? "Newline")) "NoReturn"))
              (seq (skippable*) (? #\{) (<-  name          (element))     (skippable*) (? #\}))
              (seq (skippable*) #\{     (* (<<- arguments     (element))) (skippable*) #\})
           (? (seq (skippable*) #\{     (* (<<- return-values (element))) (skippable*) #\}))))
  (bp:node* (:macro-definition :source (cons start end))
    (1 (:name         . 1) name)
    (* (:argument     . *) (nreverse arguments))
    (* (:return-value . *) (nreverse return-values))))

(defrule definition-type ()
  (bounds (start end)
          (seq "\\" "Deftype"
               (seq (skippable*) (? #\{) (<-  name        (element))     (skippable*) (? #\}))
               (seq (skippable*) #\{     (* (<<- elements (element))) (skippable*) #\})))
  (bp:node* (:type-definition :source (cons start end))
    (1 (:name . 1)    name)
    (* (:element . *) elements)))

(define-command param
  (1 :name (element)))

(define-command (kwd :kind :keyword)
  (1 :name (element)))

(define-command (bnf-rule :command-name "auxbnf")
  (1  :name    (word)) ; TODO
  (1* :element (or (row-terminator) (element))))

;;;

(defrule coloumn-separator ()
    #\&
  (bp:node* (:column-separator)))

(defrule row-terminator ()
  (seq #\\ (or "cr" "CR") (:transform (seq) nil)))

(define-command (none :command-name "None"))

(defrule table-cell/inner ()
    (bounds (start end)
      (seq (* (<<- elements (and (not (or #\& "\\span" (row-terminator))) (element))))
           (or #\& (<- span (:transform "\\span" 2))))) ; TODO should compute this
  (bp:node* (:cell :span span :source (cons start end))
    (* :element (nreverse elements))))

(defrule table-cell/last ()
    (bounds (start end)
      (seq (* (<<- elements (and (not (row-terminator)) (element)))) (row-terminator)))
  (bp:node* (:cell :source (cons start end))
    (* :element (nreverse elements))))

(defrule table-row ()
    (bounds (start end)
      (seq (* (<<- cells (table-cell/inner))) (<<- cells (table-cell/last)) (skippable*)))
  (bp:node* (:row :source (cons start end))
    (* :cell (nreverse cells))))

(defrule header ()
    (bounds (start end) (<<- elements (element)))
  (bp:node* (:header :source (cons start end))
    (* :element (nreverse elements))))

(define-command displaytwo
  (1  :caption (word))
  (1* :row     (table-row)))

(define-command displaythree
  (1  :caption (word))
  (1* :row     (table-row)))

(define-command displayfour
  (1  :caption (word))
  (1* :row     (table-row)))

(define-command displayfive
  (1  :caption (word))
  (1* :row     (table-row)))

(define-command showtwo
  (1  :caption (word))
  (1* :row     (table-row)))

(define-command showthree
  (1  :caption (word))
  (1* :row     (table-row)))

(define-command tablefigtwo
  (1  :caption (word))
  (2  :header  (header))
  (1* :row     (table-row)))

(define-command tablefigthree
  (1  :caption (word))
  (3  :header  (header))
  (1* :row     (table-row)))

(define-command tablefigfour
  (1  :caption (word))
  (4  :header  (header))
  (1* :row     (table-row)))

(define-command tablefigsix
  (1  :caption (word))
  (6  :header  (header))
  (1* :row     (table-row)))

(define-command entry
  (1 :term       (element))
  (1 :definition (element)))

(define-command tabletwo
  (2  :header  (header))
  (1* :entry   (seq (skippable*) (entry))))

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
                                   (element      '(element))
                                   (name?        t))
        (a:ensure-list name-and-options)
      `(defrule ,name ()
           (bounds (start end)
             (seq ,start-string ,@(when name? '(#\{ (<- name (word)) #\}))
                  (skippable*)
                  (* (<<- elements (and (not ,end-string)
                                        ,element)))
                  ,end-string
                  (:transform (seq) nil))) ; HACK
         (bp:node* (,kind :source (cons start end))
           ,@(when name? `((1 (:name . 1) name)))
           (* :element (nreverse elements)))))))

(defrule chapter ()
    (bounds (start end)
      (seq (seq "\\" "begin" (or #\c #\C) "hapter")
           #\{ (<- id (word)) #\}
           #\{ (<- name (word)) #\}
           #\{ (<- name2 (word)) #\}
           #\{ (<- name3 (word)) #\}
           (skippable*)
           (* (<<- elements
                   (and (not (seq "\\" "end" (or #\c #\C) "hapter")) (element))))
           (seq "\\" "end" (or #\c #\C) "hapter")
           (:transform (seq) nil)))
  (bp:node* (:chapter :source (cons start end))
    (1 (:id   . 1)  id)
    (1 (:name . 1)  name)
    (1 (:name2 . 1) name2)
    (1 (:name3 . 1) name3)
    (* :element     (nreverse elements))))

(define-command (head :command-name "Head")
  (1 :name (element)))
(define-command (head1 :command-name "HeadI")
  (1 :name (element)))
(define-environment section)
(define-environment (sub-section :keyword ("sub" "section")))
(define-environment (sub-sub-section :keyword ("sub" "sub" "section")))
(define-environment (sub-sub-sub-section :keyword ("sub" "sub" "sub" "section")))
(define-environment (sub-sub-sub-sub-section :keyword ("sub" "sub" "sub" "sub" "section")))

(defrule item-keyword ()
  (seq "\\item" (? "item") #\{ (or "\\bull" "--" (seq)) #\})) ; TODO empty bullet is used as block quote

(defrule list-item ()
    (bounds (start end)
      (seq (item-keyword)
           (skippable*)
           (seq (* (<<- body (and (not (or (item-keyword) "\\endlist")) (element)))))))
  (bp:node* (:list-item :source (cons start end))
    (* (:body . *) (nreverse body))))


(define-environment (item-list :keyword "list"
                               :name?   nil
                               :element (:transform
                                         (seq (skippable*) (<- item (or (issue) (list-item))) (skippable*))
                                         item)))

(defrule enumeration-item-keyword ()
  (seq "\\item" (? "item") #\{ (+ (guard digit-char-p)) ".}"))

(defrule enumeration-item ()
    (bounds (start end)
      (seq (enumeration-item-keyword)
           (skippable*)
           (* (<<- body (and (not (or (enumeration-item-keyword) "\\endlist")) (element))))))
  (bp:node* (:enumeration-item :source (cons start end))
    (* (:body . *) (nreverse body))))

(define-environment (enumeration-list :keyword "list"
                               :name?   nil
                               :element (:transform
                                         (seq (skippable*) (<- item (or (issue) (enumeration-item))) (skippable*))
                                         item)))

(defrule definition-item ()
    (bounds (start end)
      (seq "\\itemitem"
           (seq (skippable*) #\{ (* (<<- key (element))) (skippable*) #\})
           (skippable*)
           (* (<<- body (and (not (or "\\itemitem" "\\endlist")) (element))))))
  (bp:node* (:definition-item :source (cons start end))
    (* (:key  . *) (nreverse key))
    (* (:body . *) (nreverse body))))

(define-command (definition-item :command-name "itemitem")
    (1* :key  (element))
  (1* :body (and (not (or "\\itemitem" "\\endlist")) (element))
      :open-delimiter nil :close-delimiter nil))

(define-environment (definition-list :keyword "list"
                                     :name?   nil
                                     :element (:transform
                                               (seq (skippable*) (<- item (or (issue) (definition-item))) (skippable*))
                                               item)))

(defrule issue ()
    (bounds (start end)
      (seq "\\issue" #\{ (<- name (word)) #\} (? #\Newline)
           (* (<<- elements (and (not "\\endissue") (element))))
           "\\endissue" #\{ (<- name (word)) #\} (? #\Newline)))
  (bp:node* (:issue :source (cons start end))
    (1 :name name)
    (* :element (nreverse elements))))

(defrule editor-note ()
    (bounds (start end)
      (seq "\\editornote{" (+ (<<- editor (and (not #\:) :any))) #\:
           (+ (<<- content (and (not #\}) :any)))
           #\}))
  (let ((editor  (coerce (nreverse editor) 'string))
        (content (coerce (nreverse content) 'string)))
    (bp:node* (:editor-note :editor  editor
                            :content content
                            :bounds  (cons start end)))))

(defrule reviewer ()
  (bounds (start end)
          (seq "\\reviewer{" (? (seq (+ (<<- reviewer (and (not (or #\: #\})) :any))) #\:))
               (+ (<<- content (and (not #\}) :any)))
               #\}))
  (let ((reviewer (coerce (nreverse reviewer) 'string))
        (content  (coerce (nreverse content) 'string)))
    (bp:node* (:reviewer-note :reviewer reviewer
                              :content  content
                              :bounds   (cons start end)))))

(defrule label-name ()
    (bounds (start end)
      (seq (* (<<- content (and (not ":") :any))) #\: (? #\:)))
  (let ((content (coerce (nreverse content) 'string)))
    (bp:node* (:word :content content :bounds (cons start end)))))

(defrule label ()
    (bounds (start end)
      (seq "\\label" (skippable*) (<- name (label-name))
           (skippable*) (* (<<- elements (and (not (or "\\label" "\\endcom" "\\endissue"))
                                              (element))))))
  (bp:node* (:part :bounds (cons start end))
    (1 (:name . 1)    name)
    (* (:element . *) (nreverse elements))))

(defrule component-symbol ()
  (bounds (start end)
          (+ (<<- characters (and (not (or #\Space #\Tab #\, #\})) :any))))
  (let ((name (coerce (nreverse characters) 'string)))
    (bp:node* (:symbol :name name :source (cons start end)))))

(defrule com ()         ; TODO split name into multiple names at comma
    (bounds (start end)
      (seq "\\begincom{"
           (+ (seq (<<- names (component-symbol)) (? (seq #\, (skippable*)))))
           #\}
           (* (<<- elements (and (not "\\endcom") (element))))
           "\\endcom"))
  (format t "      Parsed component ~{~A~^, ~}~%" names)
  (bp:node* (:component :source (cons start end))
    (* (:name . *)    names)
    (* (:element . *) (nreverse elements))))

(defrule define-section ()
    (bounds (start end)
      (seq "\\DefineSection" #\{ (<- name (word)) #\}))
  (bp:node* (:define-section :source (cons start end))
    (1 (:name . 1) name)))

(defrule define-figure ()
    (bounds (start end)
      (seq "\\DefineFigure" #\{ (<- name (word)) #\}))
  (bp:node* (:define-figure :source (cons start end))
    (1 (:name . 1) name)))

;;;

(defrule glossary-entry-body ()
  (* (<<- elements (and (not (or (paragraph-break)
                                 "\\endissue")) ; HACK
                          (element))))
  (nreverse elements))

(define-command gentry
  (1  :name (word))
  (*> :body (glossary-entry-body) :open-delimiter nil :close-delimiter nil))

;;;

(defrule argument ()
    (seq (or (seq (+ (<<- level "#"))
                  (<- number (:transform (guard digit digit-char-p)
                               (digit-char-p digit))))
             "##"))
  (bp:node* (:argument :level  (length level)
                       :number number)))

(defrule parameter ()
    (seq (or (seq (+ (<<- level "#"))
                  (<- number (:transform (guard digit digit-char-p)
                               (digit-char-p digit))))
             "##")
         (* (<<- delimiter (and (not (or #\# #\{)) :any)))) ; TODO delimiter cannot be newline, i guess
  (let ((delimiter (when delimiter
                     (coerce (nreverse delimiter) 'string))))
    (bp:node* (:argument :level     (length level)
                         :number    number
                         :delimiter delimiter))))

(defrule def ()
    (bounds (start end)
      (seq "\\" (? #\g) "def"
           #\\ (<- name (identifier-with-dot))
           (* (seq (skippable*) (or (seq #\( (<<- arguments (parameter)) #\)) ; TODO the parens are probably just delimiters
                                    (<<- arguments (parameter)))))
           (skippable*) #\{ (* (<<- body (or (argument) (element)))) #\} ; TODO (argument) is basically wrong here
           (:transform (seq) nil) ; HACK work around bug
           ))
  (bp:node* (:definition :name name :source (cons start end))
    (* (:argument . *) (nreverse arguments))
    (* (:body     . *) (nreverse body))))

(defrule let-macro ()
  (bounds (start end)
          (seq "\\let"
               #\\ (<- name (identifier-with-dot))
                                        ; (* (<<- name (and (not (or #\{ #\} #\\ #\. #\# #\& #\_ (terminator))) :any))) ; TODO maybe use identifier
               (skippable*) (? #\=)
               (or (seq #\{ (+ (<<- body (and (not (or (skippable) #\})) :any))) #\})
                   (+ (<<- body (and (not (or (skippable) #\})) :any))))))
  (let (                     ; (name (coerce (nreverse name) 'string))
        (body (coerce (nreverse body) 'string)))
    (bp:node* (:definition :name name :source (cons start end))
      (1 (:body . *) (bp:node* (:word :content body))))))

(defrule other-command-application ()
    (bounds (start end)
      (or (seq "\\" (<- see? (or #\s #\S)) "ee" (and (seq (or "chapter" "section" "figure" "term")
                                                          (not (identifier)))
                                                     (<- name (identifier)))
               (<<- arguments (must (or (block*) (element)) "Macro requires an argument")))

          (seq "\\" (and (seq (or "f" "Head" "HeadI")
                              (not (identifier)))
                         (<- name (identifier)))
               (skippable*)
               (<<- arguments (must (block*) "Macro requires an argument")))

          (seq "\\" (and (or "longbookline" "DocumentNumber"
                             "vfill" "hfill" "eject" "fullhsize" "hbox")
                         (<- name (identifier))))

          (seq "\\" (and (seq (or "DefmacWithValuesNewline" "DefmacWithValues" ; TODO still needed?
                                  "DefmacNoReturn"
                                  "DefunWithValues"
                                  "more"
                                  "overfullrule"
                                  "pageno"
                                  "Vskip" "hskip" "vskip"
                                  "penalty")
                              (or (guard digit-char-p)
                                  (not (identifier))))
                         (<- name (identifier)))
               (skippable*) (<<- arguments (must (element) "Macro requires an argument")))

          (seq "\\" (and "toc" (<- name (identifier)))
               (<<- arguments (word)) (<<- arguments (block*)))

          (seq "\\" (and (seq "newif"
                              (not (identifier)))
                         (<- name (identifier)))
               (must (* (seq (* (or #\Space #\Tab)) (and (not #\Newline) (<<- arguments (name))))
                        1 2)
                     "Macro requires two arguments"))

          (seq "\\" (and (seq (or "newskip" "newdimen" "hsize" "topskip" "leftskip"
                                  "parindent" "parskip")
                              (not (identifier)))
                         (<- name (identifier)))
               (must (* (seq (* (or #\Space #\Tab)) (and (not #\Newline) (or (<<- arguments (name))
                                                                             (<<- arguments (word))))))
                     "Macro requires arguments"))

          (seq "\\"
                                        ; (+ (<<- name (and (not (or #\{ #\} #\\ #\. #\, #\$ #\# #\& (terminator))) :any)))
               (<- name (identifier))
               (skippable*)
               (* (<<- arguments (block*))))))
  (let ((name (if see?
                  (concatenate 'string "see" name)
                  name)))
    (bp:node* (:other-command-application :name name :source (cons start end))
      (* (:argument . *) arguments))))

(defrule element ()
  (or (comment)
      (spacing-command)
      (verb)
      (indexed-char)

      (b) (bf) (bold)
      (i) (ital) (it)
      (f)
      (tt)
      (rm)
      (hbox)

      (input)

      (head)
      (head1)
      (chapter)
      (section)
      (sub-section)
      (sub-sub-section)
      (sub-sub-sub-section)
      (sub-sub-sub-sub-section)
      (define-section)
      (define-figure)

      (item-list)
      (enumeration-list)
      (definition-list)

      (code)
      
      (issue)
      (editor-note)
      (reviewer)
      
      (label)
      (none)
      (com)

      (term)
      (newterm)
      (newtermidx)
      (ftype)

      (seesec)
      (seefig)
      (secref)
      (chapref)

      (reference)

      (index)

      (lambda-list-keyword)
      (definition-special-operator)
      (definition-defun/multi) (definition-defun)
      (definition-defmacro)
      (definition-type)
      (param)
      (kwd)
      (bnf-rule)


      (coloumn-separator) ; TODO only in table or command definition
      (displaytwo)
      (displaythree)
      (displayfour)
      (displayfive)
      (showtwo)
      (showthree)
      (tablefigtwo)
      (tablefigthree)
      (tablefigfour)
      (tablefigsix)
      (tabletwo)

      ;; Glossary
      (gentry)

      (def) (argument) ; TODO
      (let-macro)
      (other-command-application)
                                        ; (text)
      (block*)
      (bracket-group)
      (math-display)
      (math-group)

      (non-breaking-space)
      (paragraph-break)
      (word)

      (skippable+)))

(defrule document (filename root-kind) ; TODO file
    (bounds (start end) (* (<<- elements (element))))
  (bp:node* (root-kind :filename filename :source (cons start end))
    (* :element (nreverse elements))))

(defun make-snippet (input start end)
  (let ((raw (subseq input (max 0 (- start 20)) (min (length input) (+ end 20)))))
    (substitute #\¶ #\Newline raw)))

(defun parse-file (file &key (root-kind :file))
  (let* ((input    (a:read-file-into-string file))
         (filename (enough-namestring file))) ; TODO allow specifying base 
    (bp:with-builder ('list)
      (multiple-value-bind (result position value)
          (parser.packrat:parse `(document ,filename ,root-kind) input)
        (ecase result
          ((t)    value)
          (:fatal (let ((snippet (make-snippet input position position)))
                    (error "At ~A [~A]: ~A" position snippet value))))))))



