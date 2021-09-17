(cl:in-package #:dpans-conversion.parser)

(defgrammar issues
  (:cached? nil)
  (:class parser.packrat.grammar.string:simple-string-grammar))

(in-grammar issues)

;;; Lexical stuff

(defrule whitespace ()
  (or #\Space #\Tab))

(defrule whitespace* ()
  (* (or #\Space #\Tab)))

(defrule whitespace+ ()
  (+ (or #\Space #\Tab)))

(defrule whitespace-including-newline ()
  (or #\Space #\Tab #\Newline))

(defrule dash ()
    (bounds (start end) (seq "--" (? (<- third? #\-))))
  (bp:node* (:dash :which  (if third? :em :en)
                   :bounds (cons start end))))

(defrule in-paragraph-markup (allow-code?)
  (or (dash)
      (issue-reference 't 'nil)
      (issue-reference 'nil 't)
      (possible-reference)
      (and (:transform (seq) (unless allow-code? (:fail)))
           (code))))

(defrule chunk (allow-code?)
    (bounds (start end)
      (+ (<<- content (and (not (or #\Newline (in-paragraph-markup allow-code?)))
                           :any))))
  (let ((content (coerce (nreverse content) 'string)))
    (bp:node* (:chunk :content content :bounds (cons start end)))))

(defrule line (allow-code?)
    (bounds (start end)
      (+ (<<- elements (or (in-paragraph-markup allow-code?)
                           (chunk allow-code?)))))
  (bp:node* (:line :bounds (cons start end))
    (* (:element . *) (nreverse elements))))

(defrule paragraph-break ()
    (bounds (start end)
      (seq #\Newline (whitespace*) #\Newline))
  (bp:node* (:paragraph-break :bounds (cons start end))))

(defrule any-indent () ; TODO rename to indent
    (and (<- count (:transform (seq) 0))
         (* (or (:transform #\Space (incf count 1))
                (:transform #\Tab   (incf count 8)))))
    count)

(defrule some-indent (limit) ; TODO rename to indent
    (and (<- count (:transform (seq) 0))
         (* (or (:transform #\Space (incf count 1))
                (:transform #\Tab   (incf count 8)))
            0 limit))
  count)

(defrule indent (amount)
  (or (:transform (seq) (unless (zerop amount) (:fail))) ; TODO parser.packrat bug
      (* (whitespace) amount amount)))

(defrule indented-line (allow-code?)
    (seq (whitespace+) (<- line (line allow-code?)))
  line)

;;; Names and references

(defrule value ()
    (+ (<<- name (and (not #\Newline) :any)))
  (coerce (nreverse name) 'string))

(defrule proper-issue-character ()
  (or (and (guard alpha-char-p) (guard upper-case-p))
      (guard digit-char-p)))

(defrule issue-name ()
    (* (<<- name (or (proper-issue-character)
                     (and ;; Must not end in -, . or &.
                          (seq :any (and (not (whitespace-including-newline)) :any))
                          (or #\- #\. #\&))))
       2)
  (coerce (nreverse name) 'string))

(defrule proposal-name ()
    (+ (<<- name (or (guard alphanumericp)
                     (and ;; Must not end in - or ..
                          (seq :any (guard alphanumericp))
                          (or #\- #\.)))))
  (coerce (nreverse name) 'string))

(defrule issue-name-and-proposal-name (must-include-proposal?)
    (seq (<- issue-name (issue-name))
         (or (seq #\: (<- proposal-name (proposal-name)))
             (:transform (seq) (when must-include-proposal? (:fail)))))
  (list issue-name proposal-name))

(defrule issue-reference (must-be-explicit? must-include-proposal?) ; TODO share issue and proposal name rules with dpans grammar
    (bounds (start end)
      (seq (? (seq ; (<- explicit? (or #\I #\i)) "ssue" (whitespace*) ; TODO bug in parser.packrat: explicit? is always assigned and not rolled back when "ssue" does not match
               (or #\I #\i) "ssue" (<- explicit? (:transform (seq) t))  (whitespace*)))
           (<- (name proposal) (issue-name-and-proposal-name must-include-proposal?))))
  (when (and must-be-explicit? (not explicit?))
    (:fail))
  (bp:node* (:issue-reference :name      name
                              :proposal  proposal
                              :explicit? explicit?
                              :bounds    (cons start end))))

(defrule possible-reference ()
    (bounds (start end)
      (seq (or (seq (? (<- initial (or #\: #\& #\*)))
                    (<<- name (or (guard upper-case-p)
                                  #\- #\+ #\/ #\= #\*))
                    (+ (<<- name (or (guard upper-case-p)
                                     (guard digit-char-p)
                                     #\- #\+ #\/ #\= #\*))))
               ;; May be lower case but /must/ contain dash in that
               ;; case.
               (seq (? (<- initial (or #\: #\& #\*)))
                    (<<- name (or (guard alpha-char-p)
                                  #\+ #\/ #\= #\*))
                    (+ (<<- name (or (guard alpha-char-p)
                                     (guard digit-char-p)
                                     #\+ #\/ #\= #\*)))
                    (<<- name #\-) ; must contain dash
                    (+ (<<- name (or (guard alpha-char-p)
                                     (guard digit-char-p)
                                     #\- #\+ #\/ #\= #\*))))

               (<<- name (or #\+ #\- #\/ #\= #\T)))
           (:transform (and (or #\s (not (guard alpha-char-p))) (seq)) nil) ; HACK
           ))
  (flet ((to-string (characters)
           (coerce characters 'string)))
    (let ((name (nreverse name)))
      (multiple-value-bind (name title)
          (cond ((eql initial #\&)
                 (values (to-string name) (to-string (list* initial name))))
                ((null initial)
                 (to-string name))
                (t
                 (to-string (list* initial name))))
        (bp:node* (:possible-reference :name      name
                                       :namespace (case initial
                                                    (#\& :lambda-list-keyword))
                                       :bounds    (cons start end))
          (bp:? (:title . 1) (when title
                               (bp:node* (:chunk :content title)))))))))

;;; Code

(defrule code-symbol-ish ()
    (+ (and (not (or #\( #\) #\" #\;
                     ", "
                     (seq (and (not #\.) :any) ".)")))
            (<<- characters)))
  (when (and (some #'upper-case-p characters)
             (some #'lower-case-p characters))
    (:fail))
  (nreverse characters))

(defrule balanced-code-content ()
    (or ;; Literals starting with # and lists
        (seq (? (seq (<<- content #\#)
                     (? (<<- content (or #\c #\C #\s #\S)))))
             (<<- content #\()
             (* (<<- content (or (balanced-code-content)
                                 (code-symbol-ish))))
             (<<- content #\)))
        ;; String literals
        (seq (<<- content #\")
             (* (or (seq (<<- content #\\) (<<- content #\"))
                    (and (not #\") (<<- content))))
             (<<- content #\"))
        ;; Comments
        (seq (<<- content #\;)
             (* (and (not #\Newline) (<<- content)))
             (<<- content #\Newline)))
  (with-output-to-string (stream)
    (map nil (lambda (fragment)
               (etypecase fragment
                 (character (write-char fragment stream))
                 (string    (write-string fragment stream))))
         (a:flatten (nreverse content)))))

(defrule code ()
    (bounds (start end)
      (and #\( (<- content (balanced-code-content))))
  (bp:node* (:code :content content :bounds (cons start end))))

(bp:with-builder ('list)
  (parser.packrat:parse '(code) "(This paragraph will be superseded by cleanup issue DECLARATION-SCOPE if it passes.)"))

;;; Markup

(defrule enumeration-number ()
    (+ (<<- digits (guard digit-char-p)))
  (list (parse-integer (coerce (nreverse digits) 'string))
        :arabic))

(defrule enumeration-letter ()
    (<- character (guard alpha-char-p))
  (let ((code (char-code character)))
    (list (1+ (- code (char-code (if (lower-case-p character) #\a #\A))))
          :alpha)))

(defrule enumeration-bullet (number)
    (bounds (start end)
      (or (seq #\( (<- (number style) (or (enumeration-number) (enumeration-letter))) #\))
          (seq (<- (number style) (or (enumeration-number) (enumeration-letter))) #\.)))
  (list (- end start) style))

(defrule enumeration-item (indent number allow-code?)
    (bounds (start end)
      (seq (indent indent)
           (<- (length style) (enumeration-bullet number))
           (<<- lines (line allow-code?))
           (? (seq #\Newline
                   ;; Measure new indentation and compute next number
                   (and (seq (indent indent)
                             (<- new-indent (:transform (<- extra (some-indent length))
                                              (+ indent extra))))
                        (seq))
                   (<- next-number (:transform (seq) (1+ number)))
                   ;; Collect lines
                   (* (and (not (seq (indent indent)
                                     (or (section-label)
                                         (enumeration-bullet next-number))))
                           (or (seq (indent new-indent) (<<- lines (line allow-code?)))
                               (<<- lines (paragraph-break))
                               (seq (whitespace*) #\Newline))))))))  ; TODO make a rule for empty lines
  (bp:node* (:enumeration-item :bounds (cons start end))
    (* (:body . *) (nreverse lines))))

(defrule enumeration-list (indent allow-code?)
    (bounds (start end)
      (seq (indent indent) (and (<- extra-indent (any-indent)) (seq)) ; TODO indent should be measured beforehand
           (<- count (:transform (seq) 1))
           (<- new-indent (:transform (seq) (+ indent extra-indent)))
           (+ (seq (* (seq (whitespace*) #\Newline)) ; empty lines TODO make a rule
                   (<<- items (enumeration-item new-indent count allow-code?))
                   (:transform (seq) (incf count))))))
  (let* ((item  (first items))
         (style (when item
                  (getf (bp:node-initargs* item) :style))))
    (bp:node* (:enumeration-list :style  style
                                 :bounds (cons start end))
      (* (:element . *) (nreverse items)))))

;;; Structure

(defrule preamble ()
    (bounds (start end)
      (seq (* (<<- content (and (not (seq #\Newline (or (* "=" 3) (section-label)))) :any)))
           (? (seq (* "=" 3)
                   (* (or (whitespace) #\Newline))))
           (and (or (long-label) (short-label)) (seq))))
  (bp:node* (:preamble :content (coerce (nreverse content) 'string)
                       :bounds  (cons start end))))

(defvar *sections*
  (flet ((one-of (&rest names)
           (lambda (name)
             (some (a:curry #'string-equal name) names))))
    `((,(one-of "Aesthetics" "Esthetics")      t) ; TODO fix in source?
      ("Background"                            t)
      ("Benefits"                              t)
      ("Category"                              t) ; TODO should not happen
      ("Adoption Cost"                         t) ; TODO not sure
      ("Conversion Cost"                       t) ; TODO not sure
      ("Cost of Non-Adoption"                  t)
      ("Cost to Common Lisp implementors"      t) ; TODO
      ("Cost to Implementors"                  t)
      ("Cost to Users"                         t)
      ("Current practice"                      t)
      ("Date"                                  nil)
      ("Discussion"                            t)
      (,(one-of "Edit History" "Edit-History") nil)
      ("Example"                               t)
      ("Forum"                                 nil) ; TODO should not happen
      ("Issue"                                 nil) ; TODO should not happen
      ("Note"                                  t)   ; only seen in test-not-if-not so far
      ("Performance impact"                    t)
      ("Problem Description"                   t)
      ("Rationale"                             t)
      ("References"                            nil)
      ("Status"                                nil) ; TODO can this happen?
      ("Test Case"                             t)))) ; TODO normalize to Test Case_s_ in source?

(defun section-name? (name)
  (find-if (lambda (section)
             (destructuring-bind (test allow-code?) section
               (declare (ignore allow-code?))
               (typecase test
                 (string ; TODO should this be exact match?
                  (a:starts-with-subseq test name :test #'char-equal))
                 (function
                  (funcall test name)))))
           *sections*))

;;; Section of the form
;;;
;;; LABEL:
;;;
;;;   CONTENT

(defrule long-label ()
  (seq (+ (<<- name (and (not (or (seq (whitespace*) #\Newline)
                                  (seq #\: (whitespace*) #\Newline)))
                         :any)))
       #\: (and (seq (whitespace*) #\Newline) (seq)))
  (let* ((name (coerce (nreverse name) 'string))
         (key  (a:if-let ((index (position #\: name)))
                 (subseq name 0 index)
                 name)))
    (a:if-let ((section (section-name? key)))
      (list name (second section))
      (:fail))))

(defrule long-section (indent)
    (bounds (start end)
      (seq (indent indent)
           (<- (name allow-code?) (long-label))
           (whitespace*) #\Newline (whitespace*) #\Newline
           (* (or (and (not (seq (indent indent) (section-label)))
                       (or (<<- elements (paragraph-break))
                           (and (<- new-indent (:transform :any (+ indent 2)))
                                (<<- elements (or (enumeration-list new-indent allow-code?)
                                                  (long-section new-indent))))
                           (<<- elements (enumeration-list indent allow-code?))
                           (seq (indent indent) (<<- elements (line allow-code?)))))
                  #\Newline))))
  (bp:node* (:section :name name :bounds (cons start end))
    (* (:element . *) (nreverse elements))))

;;; Section of the form
;;;
;;; LABEL: CONTENT

(defrule short-label ()
    (seq (+ (<<- name (and (not (or #\: #\Newline)) :any)))
         #\: (and (whitespace) (seq)))
  (let* ((name (coerce (nreverse name) 'string)) ; TODO make a function
         (key  (a:if-let ((index (position #\: name)))
                 (subseq name 0 index)
                 name)))
    (a:if-let ((section (section-name? key)))
      (list name (second section))
      (:fail))))

(defrule short-section (indent)
    (bounds (start end)
      (seq (indent indent)
           (<- (name allow-code?) (short-label))
           (whitespace+) (<<- elements (line allow-code?)) #\Newline
           (* (and (or (whitespace) (not (section-label)))
                   (or (<<- elements (paragraph-break))
                       (seq (<<- elements (or (seq (indent indent)
                                                   (enumeration-list indent allow-code?))
                                              (indented-line allow-code?)))
                            #\Newline))))))
  (bp:node* (:section :name name :bounds (cons start end))
    (* (:element . *) (nreverse elements))))

;;; Special sections

(defrule section-name ()
    (bounds (start end)
      (seq "Issue:" (whitespace+) (<- value (value)) #\Newline))
  (bp:node* (:name :content value :bounds (cons start end))))

#+no (defrule section-status ()
    (bounds (start end)
      (seq "Status:" (whitespace+) (+ (<<- content (and (not (section-label)) :any)))))
  (let ((content (coerce (nreverse content) 'string)))
    content
    (bp:node* (:chunk :content content :bounds (cons start end)))))

(defrule section-related-issues ()
    (bounds (start end)
      (seq "Related" (or #\Space #\-) (or #\I #\i) "ssues:" (whitespace*)
           (* (seq (<<- references (issue-reference 'nil 'nil))
                   (? (seq (whitespace*) #\( (+ (and (not #\)) :any)) #\) (whitespace*))) ; TODO don't drop comment
                   (? (seq (or (seq #\Newline (whitespace+)) (whitespace*))
                           (? (seq #\, (whitespace*)))
                           (or (seq #\Newline (whitespace+)) (whitespace*))))))))
  (nreverse references))

(defrule section-required-issues ()
    (bounds (start end)
      (seq "Requires Issue:" (whitespace*)
           (<- reference (issue-reference 'nil 'nil))))
  (list reference))

(defrule section-forum ()
    (bounds (start end)
      (seq "Forum:" (whitespace+) (<- value (value)) #\Newline))
  (bp:node* (:forum :content value :bounds (cons start end))))

(defrule section-category ()
    (bounds (start end)
      (seq "Category:" (whitespace+) (<- value (value)) #\Newline))
  (bp:node* (:category :content value :bounds (cons start end))))

;;; Proposal section

(defrule proposal-label ()
    (seq "Proposal"
         (? (seq (whitespace*)
                 (or (seq #\( (<- (issue proposal) (issue-name-and-proposal-name 'nil)) #\))
                     (<- (issue proposal) (issue-name-and-proposal-name 'nil)))))
         (? #\:) (and (seq (whitespace*) #\Newline) (seq)))
  (list issue proposal))

(defrule proposal ()
    (bounds (start end)
      (seq (<- (issue-name proposal-name) (proposal-label))
           (whitespace*) #\Newline (whitespace*) #\Newline
           (* (or (and (not (section-label)) ; TODO (section-content) or something
                       (or (<<- elements (paragraph-break))
                           (and (<- new-indent (:transform (seq) 2))
                                (<<- elements (or (enumeration-list 0 't)
                                                  (enumeration-list new-indent 't)
                                                  (long-section new-indent))))
                           (<<- elements (line 't))))
                  #\Newline))))
  (bp:node* (:proposal :name       proposal-name
                       :issue-name issue-name
                       :bounds     (cons start end))
    (* (:element . *) (nreverse elements))))

;;;

(defrule section-label ()
  (or (proposal-label) (long-label) (short-label)))

(defrule section ()
  (or (proposal) (long-section 0) (short-section 0)))

(defrule issue (filename process)
    (bounds (start end)
      (seq (? (<- preamble (preamble)))
           (+ (or (<-  name            (section-name))
                  (<-  related-issues  (section-related-issues))
                  (<-  required-issues (section-required-issues))
                  (<-  forum           (section-forum))
                  (<-  category        (section-category))
                  (<<- sections        (section))
                  (seq (whitespace*) #\Newline)))))
  (unless (find :proposal sections :key #'bp:node-kind*)
    (cerror "Use the issue anyway" "No Proposal"))
  (bp:node* (:issue :filename filename
                    :process  process
                    :bounds   (cons start end))
    (1    (:name           . 1)    name)
    (*    (:required-issue . *)    required-issues)
    (*    (:related-issue  . *)    related-issues)
    (1    (:forum          . 1)    forum)
    (1    (:category       . 1)    category)
    (*    (:section        . *)    (nreverse sections))
    (bp:? (:preamble       . bp:?) preamble)))
