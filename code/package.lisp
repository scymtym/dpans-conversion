(cl:defpackage #:dpans-conversion
  (:use
   #:cl)

  (:local-nicknames
   (#:a         #:alexandria)

   (#:bp        #:architecture.builder-protocol)
   (#:env       #:computation.environment)

   (#:parser    #:dpans-conversion.parser)
   (#:transform #:dpans-conversion.transform))

  (:export
   #:read-specification))
