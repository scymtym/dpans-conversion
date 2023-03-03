(cl:in-package #:dpans-conversion.commandline-interface)

(opt:define-schema *schema*
  ("input-directory"       :type    'opt:directory-pathname
   :documentation
   #.(format nil "Directory from which the dpANS TeX sources and the paintext files ~
    containing X3J13 and/or WSCL issues should be read."))
  ("output-directory"      :type    'opt:directory-pathname
   :documentation
   #.(format nil "Directory into which the produced output should be written."))
  ;;
  ("format"                :type    '(member :html :sexp)
                           :default :html
   :documentation
   #.(format nil "Output format to use."))
  ;; Output options
  ("title-prefix"          :type    '(or null string)
                           :default "Well-specified Common Lisp — "
   :documentation
   #.(format nil "An optional string that should be prepended to all page titles when ~
    producing HTML."))
  ("with-sidebar"          :type    'boolean
                           :default t
   :documentation
   #.(format nil "Controls whether to generate a navigation sidebar when ~
    producing HTML."))
  ("issue-annotations"     :type    'boolean
                           :default nil
   :documentation
   #.(format nil "Controls whether to generate annotations for cleanup issues."))
  ("reviewer-annotations"  :type    'boolean
                           :default nil
   :documentation
   #.(format nil "Controls whether to generate annotations for reviewer notes."))
  ("editor-annotations"    :type    'boolean
                           :default nil
   :documentation
   #.(format nil "Controls whether to generate annotations for editor notes."))
  ("removable-annotations" :type    'boolean
                           :default nil
   :documentation
   #.(format nil "Controls whether to generate annotations for removable text."))
  ;; Debugging
  ("inspect"               :type    'boolean
                           :default t
   :documentation
   #.(format nil "Controls whether a graphical inspector for exploring the document ~
    object graph should be displayed.")))
