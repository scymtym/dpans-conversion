(cl:defpackage #:dpans-conversion.parser
  (:use
   #:cl)

  (:shadow
   #:parse-error)

  (:local-nicknames
   (#:a    #:alexandria)

   (#:bp   #:architecture.builder-protocol)
   (#:env  #:computation.environment)

   (#:base #:dpans-conversion.base))

  (:import-from #:parser.packrat
   #:defgrammar #:in-grammar
   #:defrule)

  (:import-from #:parser.packrat.grammar.base
   #:<- #:<<-
   #:guard
   #:must)

  (:import-from #:parser.packrat.grammar.sequence
   #:?
   #:seq
   #:bounds)

  (:export
   #:parse-tex-file
   #:parse-issue-file))
