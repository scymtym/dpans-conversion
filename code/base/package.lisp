(cl:defpackage #:dpans-conversion.base
  (:use
   #:cl)

  (:local-nicknames
   (#:a    #:alexandria)
   (#:sloc #:text.source-location))

  (:export
   #:annotation-mixin
   #:annotations

   #:make-annotation))
