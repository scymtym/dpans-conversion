(defsystem "dpans-conversion.clim"
  :author     "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :license    "GPLv3"

  :depends-on ("dpans-conversion"

               "mcclim"
               "clim-debugger")

  :components ((:module     "clim"
                :pathname   "code/clim"
                :serial     t
                :components ((:file "package")
                             (:file "state")

                             (:file "presentations")
                             (:file "commands")
                             (:file "display")

                             (:file "history-pane")
                             (:file "breadcrumbs-pane")
                             (:file "content-pane")

                             (:file "application")))))
