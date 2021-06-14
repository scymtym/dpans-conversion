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
    (bounds (start end) (seq #\- #\-))
  (bp:node* (:dash :which :en :bounds (cons start end))))

(defrule possible-reference ()
    (bounds (start end)
      (seq (or (seq (? (<<- name (or #\: #\& #\*)))
                    (* (<<- name (or (guard upper-case-p) #\- #\+ #\/ #\= #\*)) 2))
               (<<- name (or #\+ #\- #\/ #\= #\T)))
           (:transform (and (or #\s (not (guard alpha-char-p))) (seq)) nil) ; HACK
           ))
  (let ((name (coerce (nreverse name) 'string)))
    (bp:node* (:possible-reference :name name :bounds (cons start end)))))

(defrule chunk ()
    (bounds (start end)
      (+ (<<- content (and (not (or #\Newline
                                    (dash)
                                    (issue-reference 't)
                                    (possible-reference)))
                           :any))))
  (let ((content (coerce (nreverse content) 'string)))
    (bp:node* (:chunk :content content :bounds (cons start end)))))

(defrule line ()
    (bounds (start end)
      (+ (<<- elements (or (dash)
                           (issue-reference 't)
                           (possible-reference)
                           (chunk)))))
  (bp:node* (:line :bounds (cons start end))
    (* (:element . *) (nreverse elements))))

(defrule paragraph-break ()
    (bounds (start end)
      (seq #\Newline (whitespace*) #\Newline))
  (bp:node* (:paragraph-break :bounds (cons start end))))

(defrule indent (amount)
  (or (:transform (seq) (unless (zerop amount) (:fail))) ; TODO parser.packrat bug
      (* (whitespace) amount amount)))

(defrule indented-line ()
    (seq (whitespace+) (<- line (line)))
  line)

;;; Structure

(defrule preamble ()
    (bounds (start end)
      (seq (* (<<- content (and (not (seq #\Newline (or (* "=" 3) (label1) (label2)))) :any)))
           (? (seq (* "=" 3)
                   (* (or (whitespace) #\Newline))))
           (and (or (label1) (label2)) (seq))))
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
       (or (string-equal "Edit History")
           (string-equal "Edit-History")))
    "Example"
    "Forum" ; TODO should not happen
    "Issue" ; TODO should not happen
    "Note" ; only seen in test-not-if-not so far
    "Performance impact"
    "Problem Description"
    ,(lambda (name)
       (or (a:starts-with-subseq "Proposal " name :test #'char-equal)
           (string-equal name "Proposal")))
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

(defrule label1 ()
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

(defrule section1 (indent)
    (bounds (start end)
      (seq (indent indent)
           (<- name (label1)) (whitespace*) #\Newline (whitespace*) #\Newline
           (* (or (and (not (seq (indent indent) (or (label1) (label2))))
                       (or (<<- elements (paragraph-break))
                           (and (<- new-indent (:transform :any (+ indent 2)))
                                (<<- elements (section1 new-indent)))
                           (seq (indent indent) (<<- elements (line)))))
                  #\Newline))))
  (bp:node* (:section :name name :bounds (cons start end))
    (* (:element . *) (nreverse elements))))

;;; Section of the form
;;;
;;; LABEL: CONTENT

(defrule label2 ()
    (seq (+ (<<- name (and (not (or #\: #\Newline)) :any)))
         #\: (and (whitespace) (seq)))
  (let* ((name (coerce (nreverse name) 'string))
         (key  (a:if-let ((index (position #\: name)))
                 (subseq name 0 index)
                 name)))
    (unless (section-name? key)
      (:fail))
    name))

(defrule section2 (indent)
    (bounds (start end)
      (seq (indent indent) (<- name (label2)) (whitespace+) (<<- elements (line)) #\Newline
           (* (and (or (whitespace) (not (or (label1) (label2))))
                   (or (<<- elements (paragraph-break))
                       (seq (<<- elements (indented-line)) #\Newline))))))
  (bp:node* (:section :name name :bounds (cons start end))
    (* (:element . *) (nreverse elements))))

;;; Special sections

(defrule value ()
    (+ (<<- name (and (not #\Newline) :any)))
  (coerce (nreverse name) 'string))

(defrule issue-name ()
    (bounds (start end)
      (seq "Issue:" (whitespace+) (<- value (value)) #\Newline))
  (bp:node* (:name :content value :bounds (cons start end))))

(defrule issue-reference (must-be-explicit?) ; TODO share issue and proposal name rules with dpans grammar
    (bounds (start end)
      (seq (? (seq ; (<- explicit? (or #\I #\i)) "ssue" (whitespace*) ; TODO bug in parser.packrat: explicit? is always assigned and not rolled back when "ssue" does not match
                   (or #\I #\i) "ssue" (<- explicit? (:transform (seq) t))  (whitespace*)))
           (+ (<<- name (or (and (guard alpha-char-p) (guard upper-case-p))
                            #\-)))
           (? (seq #\: (+ (<<- proposal (or (guard alpha-char-p) #\-)))))))
  (when (and must-be-explicit? (not explicit?))
    (:fail))
  (let ((name     (coerce (nreverse name) 'string))
        (proposal (when proposal (coerce (nreverse proposal) 'string))))
    (bp:node* (:issue-reference :name      name
                                :proposal  proposal
                                :explicit? explicit?
                                :bounds    (cons start end)))))

(defrule related-issues ()
    (bounds (start end)
      (seq "Related" (or #\Space #\-) (or #\I #\i) "ssues:" (whitespace*)
           (* (seq (<<- references (issue-reference 'nil))
                   (? (seq (whitespace*) #\( (+ (and (not #\)) :any)) #\) (whitespace*))) ; TODO don't drop comment
                   (? (seq (or (seq #\Newline (whitespace+)) (whitespace*))
                           (? (seq #\, (whitespace*)))
                           (or (seq #\Newline (whitespace+)) (whitespace*))))))))
  (nreverse references))

(defrule required-issues ()
    (bounds (start end)
      (seq "Requires Issue:" (whitespace*)
           (<- reference (issue-reference 'nil))))
  (list reference))

(defrule issue-forum ()
    (bounds (start end)
      (seq "Forum:" (whitespace+) (<- value (value)) #\Newline))
  (bp:node* (:forum :content value :bounds (cons start end))))

(defrule issue-category ()
    (bounds (start end)
      (seq "Category:" (whitespace+) (<- value (value)) #\Newline))
  (bp:node* (:category :content value :bounds (cons start end))))

;;;

(defrule issue (filename)
    (bounds (start end)
      (seq (? (<- preamble (preamble)))
           (+ (or (<-  name            (issue-name))
                  (<-  related-issues  (related-issues))
                  (<-  required-issues (required-issues))
                  (<-  forum           (issue-forum))
                  (<-  category        (issue-category))
                  (<<- sections        (or (section1 0) (section2 0)))
                  (seq (whitespace*) #\Newline)))))
  (bp:node* (:issue :filename filename :bounds (cons start end))
    (1    (:name           . 1)    name)
    (*    (:required-issue . *)    required-issues)
    (*    (:related-issue  . *)    related-issues)
    (1    (:forum          . 1)    forum)
    (1    (:category       . 1)    category)
    (bp:? (:preamble       . bp:?) preamble)
    (*    (:section        . *)    (nreverse sections))))
