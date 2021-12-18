(cl:in-package #:dpans-conversion.tex)

;;; Control

(define-primitive "begingroup" ()
  :tags (:control))

(define-primitive "endgroup" ()
  :tags (:control))

(define-primitive "noexpand" (?)
  :tags (:control))

;;; Environment

(define-primitive "bye" ()
  :tags (:environment))

;;; IO

(define-primitive "immediate" (?)
  :tags (:io))

(define-primitive "write" (?)
  :tags (:io))

(define-primitive "output" (?)
  :tags (:io))

(define-primitive "shipout" (?)
  :tags (:io))

(define-primitive "closeout" (:variable)
  :tags (:io))

(define-primitive "message" (?)
  :tags (:io))

;;; Structure

(define-primitive "endgraf" ()
  :tags (:structure))

;;; Font

(define-primitive "fam" (number)
  :tags (:font))

(macrolet ((define (&rest names)
             `(progn
                ,@(map 'list (lambda (name)
                               `(define-primitive ,name (? ?)
                                  :tags (:font)))
                       names))))
  (define "textfont" "scriptfont" "scriptscriptfont"))

;;; Layout

(macrolet ((define (group arity &rest names)
             `(progn
                ,@(map 'list (lambda (name)
                               `(define-primitive ,name
                                    ,(make-list arity :initial-element '?)
                                  :tags (:layout ,group)))
                       names))))

  (define :size 0 "hss" "vss")

  (define :space 0 "negthinspace" "thinspace" "quad" "qquad")

  (define :skip 1 "hskip" "vskip"
                  "leftskip" "rightskip" "topskip"
                  "tabskip" "baselineskip"
                  "parskip" "parfillskip")

  (define :fill 0 "hfil" "hfill" "vfil" "vfill")

  (define :break 0 "break" "smallbreak" "medbreak" "goodbreak" "bigbreak")

  (define :indent/0 0 "indent" "noindent")
  (define :indent/1 1 "parindent")

  (define :misc 0 "eject" "relax" "penalty"))

(define-primitive "noalign" ()
  :tags (:layout))

(define-primitive "obeylines" ()
  :tags (:layout))

(define-primitive "ignorespaces" ()
  :tags (:layout))

(define-primitive "vglue" (?)
  :tags (:layout))

(define-primitive "vadjust" (?) ; must be expanded, not be dropped
  :tags (:layout))

(define-primitive "cleartabs" ()
  :tags (:layout))

;;; Misc/Unknown

(define-primitive "null" () ; \null is just \hbox{}.
  :tags (:misc))

;;; Math

(macrolet ((define (group arity &rest names)
             `(progn
                ,@(map 'list (lambda (name)
                               (if (consp name)
                                   (destructuring-bind (name . character) name
                                     `(define-primitive (,name :class math-symbol)
                                          ,(make-list arity :initial-element '?)
                                        :modes     (:math)
                                        :tags      (:math ,group)
                                        :character ,character))
                                   `(define-primitive ,name
                                        ,(make-list arity :initial-element '?)
                                      :modes     (:math)
                                      :tags      (:math ,group))))
                       names))))
  (define :font 1 "cal")

  (define :letter 0 ("alpha" . #\α) ("epsilon" . #\ε) ("pi" . #\π))

  (define :symbol 0 ("bullet" . #\∙) ("vert" . #\|) ("cdot" . #\⋅) ("partial" . #\∂))

  (define :function 1 "log" "sqrt")

  (define :relation 0 ("equiv" . #\≡) ("sim" . #\∼) ("neg" . #\¬)
                      ("neq" . #\≠) ("le" . #\<) ("leq" . #\≤) ("geq" . #\≥)
                      ("wedge" . #\∧) ("in" . #\∈) ("bigcup" . #\⋃) ("surd" . #\√))

  (define :arrow 0 ("rightarrow" . #\→) ("downarrow" . #\↓) ("hookleftarrow" . #\↩)
                   ("triangleright" . #\▷))

  (define :delimiter 0 ("langle" . #\⟨) ("rangle" . #\⟩)
                       ("lfloor" . #\⌊) ("rfloor" . #\⌋)
                       ("lbrack" . #\[) ("rbrack" . #\])
                       ("lcurly" . #\{) ("rcurly" . #\}))

  (define :meta-delimiter 1 "bigl" "bigr"
                            "Bigl" "Bigr"
                            "left" "right")

  (define :dot 0 ("ldots" . #\…) ("vdots" . #\⋮) ("cdots" . #\⋯))

  (define :misc/0 0 "over" "!")

  (define :misc/1 1 "buildrel" "underline"))
