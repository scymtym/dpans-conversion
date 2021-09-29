(cl:in-package #:dpans-conversion.clim)

;;; `history-pane'

(defclass history-pane (observer-mixin
                        clim:application-pane)
  ()
  (:default-initargs
   :display-function   'display-history
   :end-of-line-action :allow
   :end-of-page-action :allow
   :text-style         (clim:make-text-style nil :bold nil)
   :background         clim:+gray84+))

(defmethod clim:handle-event ((client history-pane)
                              (event  state-change-event))
  (destructuring-bind (object event &rest args) (arguments event)
    (declare (ignore object event args))
    (clim:redisplay-frame-pane (clim:pane-frame client) client :force-p t)))

(defun display-history (frame pane)
  (let ((state (state pane)))
    ;; Render the path.
    (write-string "◀" pane)
    (write-char #\Space pane)
    (write-string "▶" pane)))
