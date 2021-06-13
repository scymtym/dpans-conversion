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

  :components ((:module     "base"
                :pathname   "code/base"
                :serial     t
                :components ((:file       "package")
                             (:file       "annotation")
                             (:file       "conditions")))

               (:module     "parser"
                :pathname   "code/parser"
                :depends-on ("base")
                :serial     t
                :components ((:file       "package")
                             (:file       "environment")
                             (:file       "builtin-macros")
                             (:file       "meta-grammar")
                             ;; TeX sources
                             (:file       "grammar")
                             (:file       "tex-tables")

                             (:file       "dpans-tables")
                             (:file       "dpans-semantic")
                             ;; Issues
                             (:file       "issues")
                             ;; Interface
                             (:file       "protocol")))

               (:module     "transform"
                :pathname   "code/transform"
                :depends-on ("base")
                :serial     t
                :components ((:file       "package")
                             (:file       "protocol")
                             (:file       "utilities")
                             (:file       "environment")
                             (:file       "mixins")

                             (:file       "include-files")
                             (:file       "build-references")
                             (:file       "strip-comments")
                             (:file       "strip-tex-commands")
                             (:file       "expand-macros")))

               (:module     "html"
                :pathname   "code/html"
                :depends-on ("transform")
                :serial     t
                :components ((:file       "package")
                             ;; Utilities
                             (:file       "environment")
                             (:file       "html")
                             ;;
                             (:file       "transform")
                             (:file       "render-structure")
                             (:file       "render-typography")
                             (:file       "render-annotations")
                             (:file       "render-issue")

                             (:file       "render")
                             ;; Style and Javascript resources
                             (:static-file "style.css")
                             (:static-file "navigation.js")))

               (:module     "interface"
                :pathname   "code"
                :depends-on ("parser")
                :serial     t
                :components ((:file       "package")
                             (:file       "interface")))))
