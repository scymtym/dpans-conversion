(cl:in-package #:dpans-conversion.parser)

(defun register-builtin-macros (environment)
  (flet ((register (name argument-count)
           (case argument-count
             (:if
              (setf (env:lookup name :if environment) t))
             (:variable
              (setf (env:lookup name :variable environment) t))
             (t
              (setf (env:lookup name :macro environment) argument-count))))
         (register* (namespace name &optional argument-count)
           (case namespace
             (:if
              (setf (env:lookup name :if environment) t))
             (:variable
              (setf (env:lookup name :variable environment) t))
             (:macro
              (setf (env:lookup name :macro environment) argument-count))
             (:math
              (setf (env:lookup name :math environment) argument-count)))))
    ;; Control flow
    (register "begingroup" 0) (register "endgroup" 0)
    (register "noexpand" 1)

    ;; Output and environment
    (register "immediate" 1)
    (register "write" 1)
    (register "output" 1)
    (register "shipout" 1)
    (register "closeout" '((:variable)))
    (register "message" 1)
    (register "bye" 0)

    ;; Registers
    (register "advance" 1)

                                        ; (register "newskip" 1)

    (register "dimen" 1) ; TODO wrong; should use assignment
    (register "maxdimen" 0)

    (register "hsize" 1) (register "vsize" 1)
    (register "wd" :variable)
    (register "pagetotal" :variable)
    (register "lastbox" :variable)
    (register "llap" 1) (register "rlap" 1)

    ;; Layout
    (register "leftskip" :variable)
    (register "rightskip" :variable)
    (register "topskip" 1)
    (register "tabskip" :variable) (register "parskip" '((:variable))) (register "parfillskip" :variable)
    (register "hskip" '((:variable))) (register "vskip" 1) (register "baselineskip" 1)
    (register "offinterlineskip" 0) (register "nointerlineskip" 0)
    (register "hss" 0) (register "vss" 0)
    (register "negthinspace" 0) (register "thinspace" 0) (register "quad" 0) (register "qquad" 0)
    (register "voffset" :variable)
    (register "openup" '((t :variable))) (register "jot" :variable)

    (register "cleartabs" 0)

    (register "moveright" '((:variable)))
    (register "lower" '((:variable t)))
    (register "raise" '((:variable t)))

    (register "lastskip" 0) (register "lastskip<" 1) (register "lastskip>" 1)
    (register "prevdepth" 0) (register "prevdepth<" 1) (register "prevdepth>" 1)
    (register "removelastskip" 0)

    (register "indent" 0) (register "noindent" 0)
    (register "parindent" 1) (register "hangindent" :variable) (register "hangafter" :variable)

    (register "hfil" 0) (register "hfill" 0)
    (register "vfil" 0) (register "vfill" 0)
    (register "break" 0) (register "smallbreak" 0) (register "medbreak" 0) (register "goodbreak" 0) (register "bigbreak" 0)
    (register "eject" 0) (register "relax" 0)

    (register "kern" '((:variable)))

    (register "discretionary" 3)

    (register "ignorespaces" 0)

    (register "leaders" 2)

    (register "penalty" 1)
    (register "outputpenalty" 0)
    (register "tolerance" 1)
    (register "vbadness" 0)
    (register "vadjust" 0)

    (register "endgraf" 0)
    (register "cr" 0)

    (register "rm" 0)

    (register "magstep" 1)

    (register "null" 0)

    (register "afterassignment" 1)
    (register "overfullrule" 1)

    ;; Font
    (register "fontdimen" 1)
    (register "font" 1)
                                        ; (register "font" :variable)
    (register "fam" 8)
    (register "textfont" 1) (register "scriptfont" 1) (register "scriptscriptfont" 1)

    ;; Tables
    (register "noalign" 1)
    (register "omit"0)

    ;; Symbols
    (register "dots" 0)

    ;; Rules
    (register "vrule" 1)

    ;; Math Symbols
    (register "mmode" :if)
    (register* :math "cal" 1) ; fonts

    (register* :math "alpha" 0)
    (register* :math "epsilon" 0)       (register* :math "pi" 0)   ; letters

    (register* :math "bullet" 0)        (register* :math "vert" 0) ; symbols
    (register* :math "cdot" 0)
    (register* :math "partial" 0)

    (register* :math "log" 1)           (register* :math "sqrt" 1) ; functions

    (register* :math "equiv" 0)         (register* :math "sim" 0)  ; relations
    (register* :math "neq" 0)
    (register* :math "le" 0)
    (register* :math "leq" 0)           (register* :math "geq" 0)

    (register* :math "wedge" 0)
    (register* :math "in" 0)
    (register* :math "bigcup" 0)

    (register* :math "surd" 0)

    (register* :math "langle" 0)        (register* :math "rangle" 0) ; delimiters
    (register* :math "lfloor" 0)        (register* :math "rfloor" 0)
    (register* :math "lbrack" 0)        (register* :math "rbrack" 0)
    (register* :math "lcurly" 0)        (register* :math "rcurly" 0)

    (register* :math "bigl"   1)        (register* :math "bigr"   1)
    (register* :math "Bigl"   1)        (register* :math "Bigr"   1)
    (register* :math "left"   1)        (register* :math "right"  1)

    (register* :math "rightarrow" 0)    (register* :math "downarrow" 0) ; arrows
    (register* :math "hookleftarrow" 0)
    (register* :math "triangleright" 0)

    (register* :math "ldots" 0)         (register* :math "vdots" 0)
    (register* :math "cdots" 0)
    (register* :math "buildrel" 1)
    (register* :math "over" 0)
    (register* :math "underline" 1)
    (register* :math "!" 0) ; TODO what does this do?

    ;; Utility
    (register "uppercase" 1) (register "lowercase" 1)
    (register "string" 1) (register "expandafter" '((:variable)))

    ;; TeX builtin or not?
    (register "boxfig" 1)
    (register "leftline" 1)
    (register "pagebody" 0)
    (register "dosupereject" 0)
    (register "obeylines" 0) (register "obeyspaces" 0)
    (register "vglue" 1)
    (register "leavevmode" 0)
    (register "endit" 0)
    (register "xref" 1)
    (register "raggedright" 0)
    (register "strut" 0)

    ;; HACK these are due to use-before-definition
    (register "fixfont" 1)
    (register "par" 0)
    (register "tt" 0)
    (register "it" 0)
    (register "dummy" 0)
    (register "loopref" 0)
    (register "catothers" 0)
    (register "xbeginchapter" 1)
    (register "endTitlePage" 0)
    (register "toctrue" 0)
    (register "DefineChapter" 4)
    (register "ifshowtoc" 0)
    (register "showtoc" :if)
    (register "folio" 0)
    (register "pagenumber" 0)
    (register "closout" 1)
    (register "draft" :if)
    (register "topticks" 0)
    (register "botticks" 0)
    (register "doubleformat" 0)
    (register "clref" 1)
    (register "term" 1)
    (register "packref" 1)
    (register "logidx" 1)
    (register "chapline" 0)
    (register "bbf" 0)
    (register "docaption" 0)
    (register "finishboxfig" 0)
    (register "figlist" 1)
    (register "empty" 0)
    (register "endSection" 0)
    (register "endSimpleChapter" 0)
    (register "dobegincom" 0)
    (register "doobegincom" 0)
    (register "function" 0)
    (register "bit" 0)
    (register "word" 0)
    (register "arg" 0)
    (register "EV" 0)
    (register "paren" 1)
    (register "doformat" 0)
    (register "brfl" 0)
    (register "starparam" 1)
    (register "figref" 1)
    (register "screen" 0) (register "endscreen" 0)
    (register "keyword" 0)
    (register "truelabel" 1)
    (register "labelNone" 1)
    (register "nullabel" 0)
    (register "bf" 0)
    (register "logissue" 1)
    (register "deffigrefs" 1)
    (register "defsecrefs" 1)
    (register "indextabnote" 1)
    (register "bcindex" 1)))
