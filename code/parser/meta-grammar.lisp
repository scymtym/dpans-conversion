(cl:in-package #:dpans-conversion.parser)

;;;

(parser.packrat:defgrammar meta-grammar
  (:class   parser.packrat.grammar.sexp::sexp-grammar)
  (:cached? nil)
  (:use     parser.packrat.grammar.string::meta-grammar))

(parser.packrat:in-grammar meta-grammar)

(defrule seq/ws (context)
  (:compose (:transform (list 'seq/ws (* (<<- expressions)))
              `(seq ,@(loop :for expression :in (nreverse expressions)
                            :for first? = t :then nil
                            :unless first?
                            :collect '(skippable*)
                            :collect expression)))
            (parser.packrat.grammar.base::expression context)))

(defrule parser.packrat.grammar.base::expression (context)
  (or (seq/ws context)
      ((parser.packrat.grammar.base::expression parser.packrat.grammar.string::meta-grammar) context)))

;;;

(defclass dpans-grammar (parser.packrat.grammar.string:simple-string-grammar)
  ()
  (:default-initargs
   :meta-grammar    'meta-grammar
   :meta-start-rule 'parser.packrat.grammar.base::expression))
