(cl:defpackage #:dpans-conversion.transform
  (:use
   #:cl)

  (:local-nicknames
   (#:a    #:alexandria)

   (#:env  #:computation.environment)

   (#:bp   #:architecture.builder-protocol)

   (#:base #:dpans-conversion.base)
   (#:tex  #:dpans-conversion.tex))

  ;; Transform protocol
  (:export
   #:apply-transform
   #:peek-node
   #:transform-node)

  ;; Builder protocol and mixin
  (:export
   #:builder
   #:builder-mixin)

  ;; Reconstitution mixin
  (:export
   #:default-reconstitute-mixin
   #:reconstitute)

  ;; Peeking mixin
  (:export
   #:peeking-mixin)

  ;; Environment stack protocol and mixin
  (:export
   #:depth
   #:push-environment
   #:pop-environment
   #:call-with-environment

   #:environment-mixin)

  ;; File tracking protocol and mixin
  (:export
   #:current-file
   #:file-stack
   #:include-depth
   #:enter
   #:leave

   #:file-tracking-mixin))
