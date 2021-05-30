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
               (princ fragment stream))
         (a:flatten (nreverse content)))))

(defrule person ()
    (+ (<<- name (and (not (or #\: #\})) :any)))
  (coerce (nreverse name) 'string))

(defrule editor-note ()
    (bounds (start end)
      (seq "\\editornote{" (<- editor (person)) #\:
           (<- content (balanced-content))
           #\}))
  (bp:node* (:editor-note :editor  editor
                          :content content
                          :bounds  (cons start end))))

(defrule reviewer ()
    (bounds (start end)
      (seq "\\reviewer{" (? (<- reviewer (:transform (seq (<- temp (person)) #\:) temp))) ; HACK work around parser.packrat bug
           (<- content (balanced-content))
           #\}))
  (bp:node* (:reviewer-note :reviewer reviewer
                            :content  content
                            :bounds   (cons start end))))

;;; Issue annotation
;;;
;;; \issue{NAME} ... \endissue{NAME} is used to indicate that the
;;; X3J13 cleanup issue named NAME pertains to everything in ....

(defrule issue-annotation ()
    (bounds (start end)
      (seq "\\issue" #\{ (<- name (word)) #\} (? #\Newline)
           (* (<<- elements (and (not "\\endissue") (element))))
           "\\endissue" #\{ (<- name (word)) #\} (? #\Newline)))
  (bp:node* (:issue-annotation :bounds (cons start end))
    (1 (:name    . 1) name)
    (* (:element . *) (nreverse elements))))

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

(defrule call-syntax-special-operator ()
    (bounds (start end)
      (seq "\\" "Defspec" (? (or (seq "WithValues" (? "Newline")) "NoReturn"))
           (seq (skippable*) (? #\{) (<-  name          (element))     (skippable*) (? #\}))
           (seq (skippable*) #\{     (* (<<- arguments     (element))) (skippable*) #\})
           (? (seq (skippable*) #\{     (* (<<- return-values (element))) (skippable*) #\}))))
  (bp:node* (:call-syntax :which :special-operator :bounds (cons start end))
    (1 (:name         . *) name)
    (* (:argument     . *) arguments)
    (* (:return-value . *) return-values)))

(defrule call-syntax-defun ()
    (bounds (start end)
      (seq "\\" "Defun" (? (or (seq "WithValues" (? "Newline")) "NoReturn"))
           (seq (skippable*) (? #\{) (<-  name          (element))     (skippable*) (? #\}))
           (seq (skippable*) #\{     (* (<<- arguments     (element))) (skippable*) #\})
           (? (seq (skippable*) #\{     (* (<<- return-values (element))) (skippable*) #\}))))
  (bp:node* (:call-syntax :which :function :bounds (cons start end))
    (1 (:name         . *) name)
    (* (:argument     . *) (nreverse arguments))
    (* (:return-value . *) (nreverse return-values))))

(defrule call-syntax-defun/multi ()
    (bounds (start end)
      (seq "\\DefunMultiWithValues"
           (skippable*) #\{ (* (<<- arguments     (element)))            (skippable*) #\}
           (skippable*) #\{ (* (<<- return-values (element)))            (skippable*) #\}
           (skippable*) #\{ (* (seq "\\entry{" (<<- names (element)) #\} (skippable*))) #\}))
  (bp:node* (:call-syntax :which :function :bounds (cons start end))
    (* (:name         . *) (nreverse names))
    (* (:argument     . *) (nreverse arguments))
    (* (:return-value . *) (nreverse return-values))))

(defrule call-syntax-defgen ()
    (bounds (start end)
      (seq "\\Defgen" (? (seq "WithValues" (? "Newline")))
           (skippable*) (? #\{) (<- name (element)) (skippable*) (? #\})
           (skippable*) #\{ (* (<<- arguments     (element))) (skippable*) #\}
           (skippable*) #\{ (* (<<- return-values (element))) (skippable*) #\}))
  (bp:node* (:call-syntax :which :generic-function :bounds (cons start end))
    (1 (:name         . *) name)
    (* (:argument     . *) (nreverse arguments))
    (* (:return-value . *) (nreverse return-values))))

(defrule specialized-parameter ()
    (bounds (start end)
      (seq "\\specparam" #\{ (<- name (element)) "}{" (<- specializer (element)) #\}))
  (bp:node* (:specialized-parameter :bounds (cons start end))
    (1 (:name        . 1) name)
    (1 (:specializer . 1) specializer)))

(defrule call-syntax-defmeth ()
    (bounds (start end)
      (seq "\\Defmeth"
           (skippable*) (? #\{) (<- name (element)) (skippable*) (? #\})
           (skippable*) #\{ (* (<<- parameters (element))) (skippable*) #\}))
  (bp:node* (:call-syntax :which :method :bounds (cons start end))
    (1 (:name     . *) name)
    (* (:argument . *) (nreverse parameters))))

(defrule call-syntax-defmacro ()
    (bounds (start end)
      (seq "\\" "Defmac" (? (or (seq "WithValues" (? "Newline")) "NoReturn"))
           (seq (skippable*) (? #\{) (<-  name          (element))     (skippable*) (? #\}))
           (seq (skippable*) #\{     (* (<<- arguments     (element))) (skippable*) #\})
           (? (seq (skippable*) #\{     (* (<<- return-values (element))) (skippable*) #\}))))
  (bp:node* (:call-syntax :which :macro :bounds (cons start end))
    (1 (:name         . 1) name)
    (* (:argument     . *) (nreverse arguments))
    (* (:return-value . *) (nreverse return-values))))

(defrule call-syntax-type ()
    (bounds (start end)
      (seq "\\" "Deftype"
           (seq (skippable*) (? #\{) (<-  name        (element))     (skippable*) (? #\}))
           (seq (skippable*) #\{     (* (<<- elements (element))) (skippable*) #\})))
  (bp:node* (:type-definition :bounds (cons start end))
    (1 (:name    . 1) name)
    (* (:element . *) (nreverse elements))))

(defrule call-syntax-setf ()
    (bounds (start end)
      (seq "\\" "Defsetf"
           (skippable*) (<- name (element))
           (skippable*) #\{ (* (<<- arguments  (element))) (skippable*) #\}
           (skippable*) #\{ (<- new-value (element)) (skippable*) #\}))
  (bp:node* (:setf-definition :bounds (cons start end))
    (1 (:name      . *) name)
    (* (:argument  . *) (nreverse arguments))
    (1 (:new-value . 1) new-value)))

(defrule call-syntax-setf/multi ()
    (bounds (start end)
      (seq "\\" "DefsetfMulti"
           (skippable*) #\{ (* (<<- arguments  (element))) (skippable*) #\}
           (skippable*) #\{ (<- new-value (element)) (skippable*) #\}
           (skippable*) #\{ (* (seq "\\entry{" (<<- names (element)) #\} (skippable*))) #\}))
  (bp:node* (:setf-definition :bounds (cons start end))
    (* (:name      . *) names)
    (* (:argument  . *) (nreverse arguments))
    (1 (:new-value . 1) new-value)))

(define-command param
    (1 :name (element)))

(define-command (kwd :kind :keyword)
    (1 :name (element)))

(define-command (bnf-rule :command-name "auxbnf")
    (1  :name    (word)) ; TODO
  (1* :element (or (row-terminator) (element))))

;;; Component
;;;
;;; A component describes the value of one or more Common Lisp symbols
;;; in a particular names, for example the functions `floor', `round'
;;; or the type specifier `integer'.

(defrule label-name ()
    (bounds (start end)
      (seq (* (<<- content (and (not ":") :any))) #\: (? #\:)))
  (let ((content (coerce (nreverse content) 'string)))
    (bp:node* (:word :content content :bounds (cons start end)))))

(defrule component-label ()
    (bounds (start end)
      (seq "\\label" (skippable*) (<- name (label-name)) (skippable*)
           (* (<<- elements (and (not (or "\\label" "\\endcom" "\\endissue"))
                                 (element))))))
  (bp:node* (:part :bounds (cons start end))
    (1 (:name . 1)    name)
    (* (:element . *) (nreverse elements))))

(defrule component-name ()
    (bounds (start end)
      (or (seq (<- setf? (:transform "(setf " t))
               (+ (<<- characters (and (not #\)) :any))) #\))
          (+ (<<- characters (and (not (or #\Space #\Tab #\, #\})) :any)))))
  (let ((name (coerce (nreverse characters) 'string)))
    (bp:node* (:symbol :name name :setf? setf? :bounds (cons start end)))))

(defun print-component-name (stream name &optional colon? at?)
  (declare (ignore colon? at?))
  (let* ((initargs (bp:node-initargs* name))
         (name     (getf initargs :name))
         (setf?    (getf initargs :setf?)))
    (format stream "~:[~;setf ~]~(~A~)" setf? name)))

(defrule component ()
    (bounds (start end)
      (seq "\\begincom{"
           (+ (seq (<<- names (component-name)) (? (seq #\, (skippable*)))))
           #\}
           (* (<<- elements (and (not "\\endcom") (element))))
           "\\endcom"))
  (format t "      Parsed component ~{~/dpans-conversion.parser::print-component-name/~^, ~}~%" names)
  (bp:node* (:component :bounds (cons start end))
    (* (:name    . *) (nreverse names))
    (* (:element . *) (nreverse elements))))

;;; Glossary entry

(defrule glossary-entry-body ()
    (* (<<- elements (and (not (or (paragraph-break)
                                   "\\endissue")) ; HACK
                          (element))))
  (nreverse elements))

(define-command gentry
  (1  :name (word))
  (*> :body (glossary-entry-body) :open-delimiter nil :close-delimiter nil))
