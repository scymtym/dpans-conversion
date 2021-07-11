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

(defrule dash ()
    (bounds (start end) (seq "--" (? (<- third? #\-))))
  (bp:node* (:dash :which  (if third? :em :en)
                   :bounds (cons start end))))

(defrule possible-reference ()
    (bounds (start end)
      (seq (or (seq (? (<<- name (or #\: #\& #\*)))
                    (<<- name (or (guard upper-case-p)
                                  #\- #\+ #\/ #\= #\*))
                    (+ (<<- name (or (guard upper-case-p)
                                     (guard digit-char-p)
                                     #\- #\+ #\/ #\= #\*))))
               ;; May be lower case but /must/ contain dash in that
               ;; case.
               (seq (? (<<- name (or #\: #\& #\*)))
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
  (let ((name (coerce (nreverse name) 'string)))
    (bp:node* (:possible-reference :name name :bounds (cons start end)))))

(defrule chunk ()
    (bounds (start end)
      (+ (<<- content (and (not (or #\Newline
                                    (dash)
                                    (issue-reference 't 'nil)
                                    (issue-reference 'nil 't)
                                    (possible-reference)))
                           :any))))
  (let ((content (coerce (nreverse content) 'string)))
    (bp:node* (:chunk :content content :bounds (cons start end)))))

(defrule line ()
    (bounds (start end)
      (+ (<<- elements (or (dash)
                           (issue-reference 't 'nil)
                           (issue-reference 'nil 't)
                           (possible-reference)
                           (chunk)))))
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

(defrule indented-line ()
    (seq (whitespace+) (<- line (line)))
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
                          (seq :any (proper-issue-character))
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

(defrule enumeration-item (indent number)
    (bounds (start end)
      (seq (indent indent)
           (<- (length style) (enumeration-bullet number))
           (<<- lines (line))
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
                           (or (seq (indent new-indent) (<<- lines (line)))
                               (<<- lines (paragraph-break))
                               (seq (whitespace*) #\Newline))))))))  ; TODO make a rule for empty lines
  (bp:node* (:enumeration-item :bounds (cons start end))
    (* (:body . *) (nreverse lines))))

(defrule enumeration-list (indent)
    (bounds (start end)
      (seq (indent indent) (and (<- extra-indent (any-indent)) (seq)) ; TODO indent should be measured beforehand
           (<- count (:transform (seq) 1))
           (<- new-indent (:transform (seq) (+ indent extra-indent)))
           (+ (seq (* (seq (whitespace*) #\Newline)) ; empty lines TODO make a rule
                   (<<- items (enumeration-item new-indent count))
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
  `(,(lambda (name)
       (or (string-equal name "Aesthetics")
           (string-equal name "Esthetics"))) ; TODO fix in source?
    "Background"
    "Benefits"
    "Category" ; TODO should not happen
    "Cost of Non-Adoption"
    "Cost to Common Lisp implementors" ; TODO
    "Cost to Implementors"
    "Cost to Users"
    "Current practice"
    "Date"
    "Discussion"
    ,(lambda (name)
       (or (string-equal name "Edit History")
           (string-equal name "Edit-History")))
    "Example"
    "Forum" ; TODO should not happen
    "Issue" ; TODO should not happen
    "Note" ; only seen in test-not-if-not so far
    "Performance impact"
    "Problem Description"
    "Rationale"
    "References"
    "Status"
    "Test Case")) ; TODO normalize to Test Case_s_ in source?

(defun section-name? (name)
  (find-if (lambda (section)
             (typecase section
               (string ; TODO should this be exact match?
                (a:starts-with-subseq section name :test #'char-equal))
               (function
                (funcall section name))))
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
    (unless (section-name? key)
      (:fail))
    name))

(defrule long-section (indent)
    (bounds (start end)
      (seq (indent indent)
           (<- name (long-label)) (whitespace*) #\Newline (whitespace*) #\Newline
           (* (or (and (not (seq (indent indent) (section-label)))
                       (or (<<- elements (paragraph-break))
                           (and (<- new-indent (:transform :any (+ indent 2)))
                                (<<- elements (or (enumeration-list new-indent)
                                                  (long-section new-indent))))
                           (<<- elements (enumeration-list indent))
                           (seq (indent indent) (<<- elements (line)))))
                  #\Newline))))
  (bp:node* (:section :name name :bounds (cons start end))
    (* (:element . *) (nreverse elements))))

;;; Section of the form
;;;
;;; LABEL: CONTENT

(defrule short-label ()
    (seq (+ (<<- name (and (not (or #\: #\Newline)) :any)))
         #\: (and (whitespace) (seq)))
  (let* ((name (coerce (nreverse name) 'string))
         (key  (a:if-let ((index (position #\: name)))
                 (subseq name 0 index)
                 name)))
    (unless (section-name? key)
      (:fail))
    name))

(defrule short-section (indent)
    (bounds (start end)
      (seq (indent indent) (<- name (short-label)) (whitespace+) (<<- elements (line)) #\Newline
           (* (and (or (whitespace) (not (section-label)))
                   (or (<<- elements (paragraph-break))
                       (seq (<<- elements (or (seq (indent indent) (enumeration-list indent))
                                              (indented-line)))
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
    (print content)
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
                                (<<- elements (or (enumeration-list 0)
                                                  (enumeration-list new-indent)
                                                  (long-section new-indent))))
                           (<<- elements (line))))
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
                  ; (<-  status          (section-status))
                  (<-  related-issues  (section-related-issues))
                  (<-  required-issues (section-required-issues))
                  (<-  forum           (section-forum))
                  (<-  category        (section-category))
                  (<<- sections        (section))
                  (seq (whitespace*) #\Newline)))))
  (unless (find :proposal sections :key #'bp:node-kind*)
    (break "No Proposal"))
  (bp:node* (:issue :filename filename
                    :process  process
                    :bounds   (cons start end))
    (1    (:name           . 1)    name)
    ; (1    (:status         . 1)    status)
    (*    (:required-issue . *)    required-issues)
    (*    (:related-issue  . *)    related-issues)
    (1    (:forum          . 1)    forum)
    (1    (:category       . 1)    category)
    (bp:? (:preamble       . bp:?) preamble)
    (*    (:section        . *)    (nreverse sections))))
