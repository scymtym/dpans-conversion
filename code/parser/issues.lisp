(cl:in-package #:dpans-conversion.parser)

(defgrammar issues
  (:cached? nil)
  (:class parser.packrat.grammar.string:simple-string-grammar))

(in-grammar issues)

(defrule whitespace ()
  (or #\Space #\Tab))

(defrule whitespace* ()
  (* (or #\Space #\Tab)))

(defrule whitespace+ ()
  (+ (or #\Space #\Tab)))

(defrule line ()
    (bounds (start end)
      (seq (* (<<- content (and (not #\Newline) :any))) #\Newline))
  (let ((content (coerce (nreverse content) 'string)))
    (bp:node* (:line :content content :bounds (cons start end)))))

(defrule indented-line ()
    (seq (+ #\Space) (<- line (line)))
  line)

(defrule label1 ()
    (seq (* (<<- name (and (not (or #\Newline (seq #\: #\Newline))) :any)))
         #\: (and #\Newline (seq)))
  (coerce (nreverse name) 'string))

(defrule section1 ()
    (bounds (start end)
      (seq (<- name (label1)) #\Newline #\Newline
           (* (or (<<- lines (and (not (or (label1) (label2))) (indented-line)))
                  #\Newline))))
  (bp:node* (:section1 :name name :bounds (cons start end))
    (* (:line . *) (nreverse lines))))

(defrule label2 ()
    (seq (* (<<- name (and (not (or #\: #\Newline)) :any)))
         #\: (and (whitespace) (seq)))
  (coerce (nreverse name) 'string))

(defrule section2 ()
    (bounds (start end)
      (seq (<- name (label2)) (whitespace+) (<<- lines (line))
           (* (<<- lines (and (not (or (label1) (label2))) (indented-line))))))
  (bp:node* (:section2 :name name :bounds (cons start end))
    (* (:line . *) (nreverse lines))))

(defrule issue ()
    (bounds (start end)
      (+ (or (<<- sections (or (section1) (section2)))
             #\Newline)))
  (bp:node* (:issue :bounds (cons start end))
    (* :section (nreverse sections))))

(defvar *issue*
  (a:read-file-into-string "~/code/cl/common-lisp/dpans/issues/passed/allocate-instance"))

(bp:with-builder ('list)
  (parser.packrat:parse '(:transform (* (<<- s(or (section1) (section2) #\Newline))) s)
                        "Issue:         ALLOCATE-INSTANCE

References:    88-002R p.1-41
               89-003 p.3-35

Problem description:

  The function page for the generic function ALLOCATE-INSTANCE was
  accidentally omitted from 88-002R.

" :grammar 'issues))

(:inspect
 (architecture.builder-protocol.visualization::as-tree
  (nth-value
   2 (bp:with-builder ('list)
       (parser.packrat:parse '(issue) *issue* :grammar 'issues)))
  'list)
 :new-inspector? t)
