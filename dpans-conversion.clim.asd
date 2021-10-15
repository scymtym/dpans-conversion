(defsystem "dpans-conversion.clim"
  :author     "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :license    "GPLv3"

  :depends-on ("dpans-conversion")

  :components ((:module     "clim"
                :pathname   "code/clim"
                :serial     t
                :components ((:file "package")
                             (:file "presentation")
                             (:file "state")
                             (:file "breadcrumbs-pane")
                             (:file "content-pane")
                             (:file "application")))))