(cl:in-package #:dpans-conversion.clim)

;;; Utilities

(defun link (node stream title)
  (clim:with-output-as-presentation (stream node 'link)
    (etypecase title
      (string   (write-string title stream))
      (function (funcall title stream)))))

;;; `link'

(clim:define-presentation-type link ()
  :inherit-from t)

(clim:define-presentation-method clim:present ((object t)
                                               (type   link)
                                               (stream t)
                                               (view   clim:textual-view)
                                               &key)
  (write-string "<link>" stream))

;;; `expression' and `listing'

(clim:define-presentation-type expression ()
  :inherit-from t)

(clim:define-presentation-type listing ()
  :inherit-from t)

;;; `search-result'

(clim:define-presentation-type search-result ()
  :inherit-from 'link)
