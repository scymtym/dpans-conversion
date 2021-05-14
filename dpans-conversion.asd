(defsystem "dpans-conversion"
  :author     "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :license    "GPLv3"

  :depends-on ("parser.packrat"
               "parser.packrat.grammar.string"

               "architecture.builder-protocol"
               "architecture.builder-protocol.visualization" ; debugging

               "computation.environment"
               "computation.environment.visualization" ; debugging

               "cxml") ; html output

  :components ((:module     "parser"
                :pathname   "code/parser"
                :serial     t
                :components ((:file       "package")
                             (:file       "grammar")
                             (:file       "semantic")
                             (:file       "issues")))

               (:module     "transform"
                :pathname   "code/transform"
                :serial     t
                :components ((:file       "package")
                             (:file       "protocol")
                             (:file       "mixins")
                             (:file       "strip-comments")
                             (:file       "strip-tex-commands")
                             (:file       "expand-macros")))

               (:module     "html"
                :pathname   "code/html"
                :serial     t
                :components ((:file       "package")
                             (:file       "environment")
                             (:file       "render")))))
