(cl:in-package #:dpans-conversion.parser)

(in-grammar dpans)

;;; Editor and reviewer notes

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
      (seq "\\reviewer{" (? (seq (<- reviewer (person)) #\:))
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

(defrule component-symbol ()
    (bounds (start end)
      (or (seq "(setf " (+ (<<- characters (and (not #\)) :any))) #\))
          (+ (<<- characters (and (not (or #\Space #\Tab #\, #\})) :any)))))
  (let ((name (coerce (nreverse characters) 'string)))
    (bp:node* (:symbol :name name :bounds (cons start end)))))

(defrule component ()
    (bounds (start end)
      (seq "\\begincom{"
           (+ (seq (<<- names (component-symbol)) (? (seq #\, (skippable*)))))
           #\}
           (* (<<- elements (and (not "\\endcom") (element))))
           "\\endcom"))
  (format t "      Parsed component ~{~A~^, ~}~%" names)
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
