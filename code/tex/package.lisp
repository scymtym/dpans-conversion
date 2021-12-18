(cl:defpackage #:dpans-conversion.tex
  (:use
   #:cl)

  (:shadow
   #:character)

  (:local-nicknames
   (#:a         #:alexandria)

   (#:env       #:computation.environment))

  ;; Primitive protocol
  (:export
   #:name
   #:arguments
   #:modes
   #:tags)

  ;; Math symbol protocol
  (:export
   #:character)

  ;; Primitive set protocol
  (:export
   #:find-primitive))
