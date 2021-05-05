(cl:defpackage #:dpans-conversion
  (:use
   #:cl)

  (:local-nicknames
   (#:a   #:alexandria)

   (#:env #:computation.environment)

   (#:bp  #:architecture.builder-protocol))

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
   #:bounds))
