(cl:in-package #:dpans-conversion.parser)

(defgrammar dpans
  (:class   dpans-grammar)
  (:cached? nil))

(in-grammar dpans)

(defrule tabbed-row-start ()
    (bounds (start end) "\\+" (:transform (seq) nil))
  (bp:node* (:row-start :bounds (cons start end))))

(defrule column-separator () ; TODO this lexical level stuff
    (bounds (start end) #\&)
  (bp:node* (:column-separator :bounds (cons start end))))

(defrule row-terminator () ; TODO these are also needed for TeX tables
    (bounds (start end)
      "\\cr" (and (not (identifier)) (:transform (seq) :nil)))
  (bp:node* (:row-terminator :bounds (cons start end))))

(defrule span ()
  "\\span")

;;; This is used in the template section of \\halign.
(defrule placeholder ()           ; TODO placeholder may not be needed
  (+ #\#))

;;; \settabs table
;;;
;;; An invocation of \setabs followed by lines of the form
;;; \+ … & … & … \cr

(defrule settabs-cell (environment)
  (bounds (start end)
          (seq (* (seq (and (not (or (column-separator) (row-terminator)))
                            (<<- elements (element environment)))
                       (skippable*)))
               (? (column-separator))))
  (bp:node* (:cell :bounds (cons start end))
    (* (:element . *) (nreverse elements))))

(defrule settabs-row (environment)
    (bounds (start end)
      (seq/ws (tabbed-row-start)
              (* (and (not (row-terminator))
                      (<<- cells (settabs-cell environment))))
              (row-terminator)))
  (bp:node* (:row :bounds (cons start end))
    (* (:cell . *) (nreverse cells))))

(defrule settabs (environment)
    (bounds (start end)
      (seq/ws "\\settabs"
              (* (seq (skippable*) (<<- rows (settabs-row environment))))))
  (bp:node* (:table :kind :settabs)
    (* (:row . *) (nreverse rows))))

(when nil
  (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
    (register-builtin-macros env)
    (bp:with-builder ('list)
      (parser.packrat:parse
       `(settabs ,env)
       "\\settabs\\+\\cr
\\+a&b&c\\cr"))))

;;; \halign table
;;;
;;; An invocation of the \halign macro has the following structure:
;;;
;;;   \halign[|to TO | spread SPREAD]{
;;;     … # … & … # … & … # … \cr
;;;     … & … & … \cr
;;;     ⋮
;;;   }
;;;
;;; The first line specifies a template in which # are placeholders.

(defrule halign-cell (environment)
  (bounds (start end)
          (seq/ws (* (<<- elements (and (not (or (column-separator) (row-terminator) #\})) ; TODO } needed?
                                        (or (placeholder) (element environment))))) ; TODO is placeholder an element?
                  (or (column-separator)
                      (and (or (row-terminator) #\}) (seq)))))
  (bp:node* (:cell :bounds (cons start end))
    (* (:element . *) (nreverse elements))))

(defrule halign-row (environment)
    (bounds (start end)
      (seq (* (seq (<<- cells (and (not (or (row-terminator) #\})) (halign-cell environment))) (skippable*)))
           (or (row-terminator) (and #\} (seq))))) ; TODO is the terminator required?
  (bp:node* (:row :bounds (cons start end))
    (* (:cell . *) (nreverse cells))))

(defrule halign (environment)
    (bounds (start end)
      (seq/ws "\\halign"
              (or (seq/ws "to"     (<- to (name)))
                  (seq/ws "spread" (<- spread (name)))
                  (seq))
              (? (seq/ws #\{ (* (seq (<<- rows (and (not #\}) (halign-row environment))) (skippable*))) #\})))) ; TODO really optional?
  (bp:node* (:table :kind   :halign
                    :bounds (cons start end))
    (bp:? (:to     . 1) to)
    (bp:? (:spread . 1) spread)
    (*    (:row    . *) (nreverse rows))))

(when nil
 (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
   (register-builtin-macros env)
   (setf (env:lookup "fullhsize" :variable env) t)
   (setf (env:lookup "qquad" :macro env) 0)
   (bp:with-builder ('list)
     (parser.packrat:parse
      `(document "foo" 1 ,env)
      "\\hbox{{\\def\\entry##1{##1&\\cr}%
\\vbox{\\halign{\\function ## {\\arg #1}\\hfil&\\quad\\EV\\ {\\arg #2}##\\cr#4}}}\\qquad\\vrule\\qquad
{\\def\\entry##1{##1\\cr}%
\\vbox{\\halign{\\function (setf (## {\\arg #1}) {\\arg #3})\\cr#4}}}}"))))

(when nil
 (let ((env (make-instance 'env:lexical-environment :parent **meta-environment**)))
   (register-builtin-macros env)
   (setf (env:lookup "toc" :if env) t)
   (setf (env:lookup "tocfile" :macro env) 0)
   (setf (env:lookup "tocfalse" :macro env) 0)
   (setf (env:lookup "caption" :macro env) 1)
   (setf (env:lookup "endfig" :macro env) 0)
   (bp:with-builder ('list)
     (parser.packrat:parse
      `(document "somefile" 0 ,env)
      "\\boxfig
{\\dimen0=.75pc
\\tabskip \\dimen0 plus .5 fil
\\offinterlineskip
\\halign to \\hsize {\\strut#\\hfil\\tabskip \\dimen0 plus 1fil&#\\hfil\\tabskip
\\dimen0 plus .5 fil&#\\hfil\\tabskip \\dimen0 plus 1fil&#\\hfil\\tabskip \\dimen0 plus 1fil
&#\\hfil&#\\hfil&#\\hfil\\cr
\\noalign{\\vskip -11pt}
\\hfil\\b{CT} &\\hfil\\b{LT} &\\hfil\\b{E} &\\hfil\\b{Mode}&\\hfil\\b{Action}&\\hfil\\b{New Mode}\\cr
\\noalign{\\hrule}
Yes&Yes&\\hfil---&\\hfil---&Process&compile-time-too\\cr
No&Yes&Yes&\\hfil CTT&Process&compile-time-too\\cr
No&Yes&Yes&\\hfil NCT&Process&not-compile-time\\cr
No&Yes&No&\\hfil---&Process&not-compile-time\\cr
Yes&No&\\hfil---&\\hfil---&Evaluate&\\hfil---\\cr
No&No&Yes&\\hfil CTT&Evaluate&\\hfil---\\cr
No&No&Yes&\\hfil NCT&Discard&\\hfil---\\cr
No&No&No&\\hfil---&Discard&\\hfil---\\cr
\\noalign{\\vskip -9pt}
}}
\\caption{EVAL-WHEN processing}
\\endfig
"))))
