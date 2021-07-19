(cl:defpackage #:dpans-conversion.commandline-interface
  (:use
   #:cl)

  (:local-nicknames
   (#:a   #:alexandria)

   (#:opt #:configuration.options))

  (:export
   #:main))
