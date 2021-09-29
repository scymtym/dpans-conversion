(cl:in-package #:dpans-conversion.parser)

(in-grammar dpans)

;;; Editor and reviewer notes
;;;
;;; Commands with syntax \editornote{EDITOR: CONTENT} and
;;; \reviewer{[REVIEWER:] CONTENT}. CONTENT is uninterpreted except
;;; for counting {} pairs.

(defrule balanced-content ()
    (* (or (seq (<<- content #\{)
                (<<- content (balanced-content))
                (<<- content #\}))
           (<<- content (and (not #\}) :any))))
  (with-output-to-string (stream)
    (map nil (lambda (fragment)
               (etypecase fragment
                 (character (write-char fragment stream))
                 (string    (write-string fragment stream))))
         (a:flatten (nreverse content)))))

(defrule person ()
    (+ (<<- name (and (not (or #\: #\})) :any)))
  (coerce (nreverse name) 'string))

(defrule editor-note (environment)
    (bounds (start end)
      (seq/ws "\\editornote{" (<- editor (person)) #\:
              (<- content (balanced-content))
              #\}))
  (bp:node* (:editor-note :editor  editor
                          :content content
                          :bounds  (cons start end))))

(defrule reviewer (environment)
    (bounds (start end)
      (seq/ws "\\reviewer{" (? (<- reviewer (:transform (seq/ws (<- temp (person)) #\:) temp))) ; HACK work around parser.packrat bug
              (<- content (balanced-content))
              #\}))
  (bp:node* (:reviewer-note :reviewer reviewer
                            :content  content
                            :bounds   (cons start end))))

;;; Issue annotation
;;;
;;; \issue{NAME} ... \endissue{NAME} is used to indicate that the
;;; X3J13 cleanup issue named NAME pertains to everything in ....

(defrule issue-name ()
    (seq (+ (<<- name (and (not (or #\: #\})) :any)))
         (? (seq #\: (+ (<<- proposal (and (not #\}) :any))))))
  (list (coerce (nreverse name) 'string)
        (when proposal
          (coerce (nreverse proposal) 'string))))

(defrule issue-annotation (environment)
    (bounds (start end)
      (seq "\\issue" #\{ (<- (name proposal) (issue-name)) #\} (? #\Newline)
           (or (seq #+dpans-debug (:transform (seq) (format *trace-output* "~V@T[ issue ~A~%" *depth* name) (incf *depth*))
                    (* (<<- elements (and (not "\\endissue") (element environment))))
                    "\\endissue" #\{ (<- (name proposal) (issue-name)) #\} (? #\Newline)
                    #+dpans-debug (:transform (seq) (decf *depth*) (format *trace-output* "~V@T] issue ~A~%" *depth* name)))
               #+dpans-debug (:transform (seq) (decf *depth*) (format *trace-output* "~V@TX issue ~A~%" *depth* name) (:fail))
               #-dpans-debug (:transform (seq) (:fail)))))
  (bp:node* (:issue-annotation :name     name
                               :proposal proposal
                               :bounds   (cons start end))
    (* (:element . *) (nreverse elements))))

;;; Name

(defrule component-name () ; TODO rename to just name? or to symbol?
    (bounds (start end)
      (or (seq (<- setf? (:transform "(setf " t))
               (+ (<<- characters (and (not #\)) :any))) #\))
          (+ (or (seq #\$ (* (<<- characters (and (not #\$) :any))) #\$) ; Read 1$-$ as 1-
                 (<<- characters (and (not (or #\Space #\Tab #\Newline #\, #\{ #\}))
                                      (or (seq #\\ :any)
                                          :any)))))))
  (let ((name (coerce (nreverse characters) 'string)))
    (bp:node* (:symbol :name name :setf? setf? :bounds (cons start end)))))

#+old (defrule entry () ; TODO integrate with `name' and/or `symbol'?
    (bounds (start end)
      (seq "\\entry{" (or (seq/ws #\{ (+ (or #\$ (<<- characters (and (not #\}) :any)))) #\}) ; TODO optional block is a hack
                          (+ (or #\$ (<<- characters (and (not #\}) :any)))))
           #\}))
  (let ((name (coerce (nreverse characters) 'string)))
    (bp:node* (:symbol :name name :bounds (cons start end)))))

(defrule entry (environment)
    (bounds (start end)
            (seq "\\entry{"
                 (or (seq/ws #\{ (<- name (component-name)) #\}) ; TODO optional block is a hack
                     (<- name (component-name)))
                 #\}))
  name)

(defrule entry-list (environment)
    (* (seq (or (<<- names (entry environment))
                (user-macro-application environment))
            (skippable* environment)))
  (nreverse names))

;;; Syntax

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

(defrule call-syntax-name (environment)
    (must (or (seq/ws #\{ (<- name (or (argument environment) (component-name))) #\}) ; TODO handle this better
              (<- name (or (argument environment) (component-name))))
          "must be a name, optionally within curly braces")
  name)

(defrule call-syntax-special-operator (environment)
    (bounds (start end)
      (seq/ws (seq "\\Defspec" (? (or (seq "WithValues" (? "Newline")) "NoReturn")))
              (<- name (call-syntax-name environment))
              #\{ (* (<<- arguments (element environment))) #\}
              (? (seq/ws #\{ (* (seq (skippable* environment) (<<- return-values (element environment)))) #\}))))
  (bp:node* (:call-syntax :which :special-operator :bounds (cons start end))
    (1 (:name         . 1) name)
    (* (:argument     . *) (nreverse arguments))
    (* (:return-value . *) return-values)))

(defrule call-syntax-defun (environment)
    (bounds (start end)
      (seq/ws (seq "\\Defun" (? (or (seq "WithValues" (? "Newline")) "NoReturn")))
              (<- name (call-syntax-name environment))
              #\{ (* (<<- arguments (element environment))) #\}
              (? (seq #\{ (* (seq (skippable* environment) (<<- return-values (element environment)))) #\}))))
  (bp:node* (:call-syntax :which :function :bounds (cons start end))
    (1 (:name         . *) name)
    (* (:argument     . *) (nreverse arguments))
    (* (:return-value . *) (nreverse return-values))))

(defrule call-syntax-defun/multi (environment)
    (bounds (start end)
      (seq/ws "\\DefunMultiWithValues"
              #\{ (* (<<- arguments (element environment))) #\}
              #\{ (* (<<- return-values (element environment))) #\}
              #\{ (<- names (entry-list environment)) #\}))
  (bp:node* (:call-syntax :which :function :bounds (cons start end))
    (* (:name         . *) names)
    (* (:argument     . *) (nreverse arguments))
    (* (:return-value . *) (nreverse return-values))))

(when nil
  (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
    (register-builtin-macros env)
    (bp:with-builder ('list)
      (parser.packrat:parse
       `(call-syntax-defun/multi ,env)
       "\\DefunMultiWithValues {string1 string2 {\\key} start1 end1 start2 end2}
{mismatch-index}
{\\entry{string/$=$}
\\entry{string$<$}
\\entry{string$>$}
\\entry{string$<=$}
\\entry{string$>=$}}"))))

(when nil
  (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
    (register-builtin-macros env)
    (setf (env:lookup "plus" :macro env) 1)
    (bp:with-builder ('list)
      (parser.packrat:parse
       `(call-syntax-defun/multi ,env)
       "\\DefunMultiWithValues {{\\rest} \\plus{characters}}
{generalized-boolean}
{\\entry{{char$=$}}
\\entry{{char$/=$}}
\\entry{{char$<$}}
\\entry{{char$>$}}
\\entry{{char$<=$}}
\\entry{{char$>=$}}
\\noalign{\\vskip 5pt}
\\entry{char-equal}
\\entry{char-not-equal}
\\entry{char-lessp}
\\entry{char-greaterp}
\\entry{char-not-greaterp}
\\entry{char-not-lessp}}
"))))

(when nil
  (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
    (register-builtin-macros env)
    (bp:with-builder ('list)
      (parser.packrat:parse
       `(call-syntax-defun/multi ,env)
       "\\DefunMultiWithValues {number {\\opt} divisor} {quotient, remainder}
{\\entry{floor}
\\entry{ffloor}
\\entry{ceiling}
\\entry{fceiling}
\\entry{truncate}
\\entry{ftruncate}
\\entry{round}
\\entry{fround}}"))))

(defrule call-syntax-accessor/multi (environment)
    (bounds (start end)
      (seq/ws "\\DefunMultiAccessorWithValues"
              #\{ (* (<<- arguments (element environment))) #\}
              #\{ (* (<<- return-values (element environment))) #\}
              #\{ (<- new-value (element environment)) #\}
              #\{ (<- names (entry-list environment)) #\}))
  (bp:node* (:call-syntax :which :accessor :bounds (cons start end))
    (* (:name         . *) names)
    (* (:argument     . *) (nreverse arguments))
    (1 (:new-value    . 1) new-value)
    (* (:return-value . *) (nreverse return-values))))

(when nil
  (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
    (register-builtin-macros env)
    (setf (env:lookup "blankline" :macro env) 0)
    (bp:with-builder ('list)
      (parser.packrat:parse
       `(call-syntax-accessor/multi ,env)
"\\DefunMultiAccessorWithValues {x} {object} {new-object}
       {\\entry{car}
       \\entry{cdr}
       \\blankline
       \\entry{caar}
       \\entry{cadr}
       \\entry{cdar}
       \\entry{cddr}
       \\blankline
       \\entry{caaar}
       \\entry{caadr}
       \\entry{cadar}
       \\entry{caddr}
       \\entry{cdaar}
       \\entry{cdadr}
       \\entry{cddar}
       \\entry{cdddr}
       \\blankline
       \\entry{caaaar}
       \\entry{caaadr}
       \\entry{caadar}
       \\entry{caaddr}
       \\entry{cadaar}
       \\entry{cadadr}
       \\entry{caddar}
       \\entry{cadddr}
       \\entry{cdaaar}
       \\entry{cdaadr}
       \\entry{cdadar}
       \\entry{cdaddr}
       \\entry{cddaar}
       \\entry{cddadr}
       \\entry{cdddar}
       \\entry{cddddr}}"))))

(defrule call-syntax-defgen (environment)
    (bounds (start end)
      (seq/ws (seq "\\Defgen" (? (seq "WithValues" (? "Newline"))))
              (<- name (call-syntax-name environment))
              #\{ (* (<<- arguments     (element environment))) #\}
              #\{ (* (<<- return-values (element environment))) #\}))
  (bp:node* (:call-syntax :which :generic-function :bounds (cons start end))
    (1 (:name         . 1) name)
    (* (:argument     . *) (nreverse arguments))
    (* (:return-value . *) (nreverse return-values))))

(defrule specializer (environment)
    (bounds (start end)
       (or (and (not (or #\\ #\{ #\} #\' #\( #\)))
                (<- type (element environment)))
          (seq/ws (? "\\f{") #\( "eql"
                  (+ (<<- value (and (not #\))
                                     (element environment))))
                  #\) (? #\}))))
  (if type
      (bp:node* (:typeref :bounds (cons start end))
        (1 (:name . 1) type))
      (bp:node* (:eql-specializer :bounds (cons start end))
        (* (:value . *) (nreverse value)))))

(defrule specialized-parameter (environment)
    (bounds (start end)
      (seq/ws "\\specparam" #\{ (<- name (element environment)) #\}
              #\{ (<- specializer (specializer environment)) #\}))
  (bp:node* (:specialized-parameter :bounds (cons start end))
    (1 (:name        . 1) name)
    (1 (:specializer . 1) specializer)))

(defrule call-syntax-defmeth (environment)
    (bounds (start end)
      (seq/ws "\\Defmeth" (<- name (call-syntax-name environment))
              #\{ (* (<<- parameters (element environment))) #\}))
  (bp:node* (:call-syntax :which :method :bounds (cons start end))
    (1 (:name     . 1) name)
    (* (:argument . *) (nreverse parameters))))

(defrule call-syntax-defmacro (environment)
    (bounds (start end)
      (seq/ws (seq "\\Defmac" (? (or (seq "WithValues" (? "Newline")) "NoReturn")))
              (<- name (call-syntax-name environment))
              #\{ (* (<<- arguments (element environment))) #\}
              (? (seq #\{ (* (<<- return-values (element environment))) #\}))))
  (bp:node* (:call-syntax :which :macro :bounds (cons start end))
    (1 (:name         . 1) name)
    (* (:argument     . *) (nreverse arguments))
    (* (:return-value . *) (nreverse return-values))))

(defrule call-syntax-type (environment)
    (bounds (start end)
      (seq/ws "\\Deftype" (<- name (call-syntax-name environment))
              #\{ (* (<<- elements (element environment))) #\}))
  (bp:node* (:call-syntax :which :type :bounds (cons start end))
    (1 (:name    . 1) name)
    (* (:element . *) (nreverse elements))))

(defrule call-syntax-setf (environment)
    (bounds (start end)
      (seq/ws "\\Defsetf" (<- name (call-syntax-name environment))
              #\{ (* (<<- arguments (element environment))) #\}
              #\{ (<- new-value (element environment)) #\}))
  (bp:node* (:call-syntax :which :setf :bounds (cons start end))
    (1 (:name      . *) name)
    (* (:argument  . *) (nreverse arguments))
    (1 (:new-value . 1) new-value)))

(defrule call-syntax-setf/multi (environment)
    (bounds (start end)
      (seq/ws "\\DefsetfMulti"
              #\{ (* (<<- arguments (element environment))) #\}
              #\{ (<- new-value (element environment)) #\}
              #\{ (<- names (entry-list environment)) #\}))
  (bp:node* (:call-syntax :which :setf :bounds (cons start end))
    (* (:name      . *) names)
    (* (:argument  . *) (nreverse arguments))
    (1 (:new-value . 1) new-value)))

(defrule call-syntax (environment)
  (or (call-syntax-special-operator environment)
      (call-syntax-accessor/multi environment)
      (call-syntax-defun/multi environment)      (call-syntax-defun environment)
      (call-syntax-defgen environment)
      (call-syntax-defmeth environment)
      (specialized-parameter environment)
      (call-syntax-defmacro environment)
      (call-syntax-type environment)
      (call-syntax-setf/multi environment)       (call-syntax-setf environment)))

(define-command param
    (1 :name (element environment)))

(define-command (kwd :kind :keyword)
    (1 :name (element environment)))

(defrule row-terminator* (environment) ; TODO disallow in other contexts?
    (and (:transform (seq) (env:lookup :bnf-rule :traversal environment
                                                 :if-does-not-exist nil))
         "\\CR")
  nil)

(defrule bnf-rule (environment)
    (bounds (start end)
      (seq/ws "\\auxbnf"
               #\{ (<- name (chunk environment)) #\}
               #\{
               (and (<- new-environment (:transform (seq)
                                          (env:augmented-environment
                                           environment '((:bnf-rule . :traversal)) '(t))))
                    (* (<<- elements (element new-environment))))
               #\}))
  (bp:node* (:bnf-rule :bounds (cons start end))
    (1 (:name    . 1) name)
    (* (:element . *) (nreverse elements))))

;;; Component
;;;
;;; A component describes the value of one or more Common Lisp symbols
;;; in a particular names, for example the functions `floor', `round'
;;; or the type specifier `integer'.

(defrule label-name ()
    (bounds (start end)
      (seq (* (<<- content (and (not (or #\: #\{)) :any))) #\: (? #\:)))
  (let ((content (coerce (nreverse content) 'string)))
    (bp:node* (:chunk :content content :bounds (cons start end)))))

(defrule component-label (environment)
    (bounds (start end)
      (seq/ws "\\label" (<- name (label-name))
              (* (<<- elements (and (not (or "\\label" "\\endcom" "\\endissue"))
                                    (element environment))))))
  (bp:node* (:part :bounds (cons start end))
    (1 (:name    . 1) name)
    (* (:element . *) (nreverse elements))))

(define-command (none :command-name "None"))

(defun print-component-name (stream name &optional colon? at?)
  (declare (ignore colon? at?))
  (let* ((initargs (bp:node-initargs* name))
         (name     (getf initargs :name))
         (setf?    (getf initargs :setf?)))
    (format stream "~:[~;setf ~]~(~A~)" setf? name)))

(defrule component (environment)
    (bounds (start end)
      (seq "\\begincom{"
           (must (+ (seq (<<- names (component-name)) (? (seq #\, (skippable* environment)))))
                 "must be a component name")
           #\}
           (* (<<- elements (and (not "\\endcom") (element environment))))
           (must "\\endcom" "must be a complete component")))
  (format t "      Parsed component ~{~/dpans-conversion.parser::print-component-name/~^, ~}~%" names)
  (bp:node* (:component :bounds (cons start end))
    (* (:name    . *) (nreverse names))
    (* (:element . *) (nreverse elements))))

;;; Glossary entry

(defrule glossary-entry-body (environment)
    (* (<<- elements (and (not (or "\\gentry"
                                   "\\indextab"
                                   "\\endlist"
                                   "\\endissue")) ; HACK
                          (element environment))))
  (nreverse elements))

(defrule gentry (environment)
    (bounds (start end)
      (seq/ws "\\gentry" #\{ (<- name (chunk environment)) #\}
              (<- body (glossary-entry-body environment))))
  (bp:node* (:gentry :bounds (cons start end))
    (1 (:name . 1) name)
    (* (:body . *) body)))

(defrule glossary-section (environment)
    (bounds (start end)
      (seq (seq #\\ (? "first") "indextab") #\{ (<- name (chunk environment)) #\}
           (* (or (skippable+ environment)
                  (<<- entries (or (issue-annotation environment)
                                   (editor-note environment)
                                   (gentry environment)))))
           (:transform (seq) (format *trace-output* "Parsed glossary section ~A~%" name))))
  (bp:node* (:section :level  1
                      :bounds (cons start end))
    (1 (:name    . 1) name)
    (* (:element . *) (nreverse entries))))

(defrule glossary-list (environment)
  (seq/ws "\\beginlist"
          (* (<<- sections (glossary-section environment)))
          "\\endlist")
  (bp:node* (:splice)
    (* (:element . *) (nreverse sections))))
