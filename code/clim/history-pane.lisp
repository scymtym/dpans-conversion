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

;;;

(clim:define-presentation-type back-button ()
  :inherit-from t)

(clim:define-presentation-to-command-translator back-button->com-back
    (back-button com-back viewer
     :tester ((object)
              (clim:command-enabled 'com-back clim:*application-frame*))
     :menu   nil)
    (object)
  '())

(clim:define-presentation-method clim:present ((object t)
                                               (type   back-button)
                                               (stream clim:extended-output-stream)
                                               (view   t)
                                               &key)
  (clim:with-drawing-options (stream :ink (if (back-possible? object)
                                              clim:+black+
                                              clim:+gray60+))
    (write-string "◀" stream)))

(clim:define-presentation-type forward-button ()
  :inherit-from t)

(clim:define-presentation-to-command-translator forward-button->com-forward
    (forward-button com-forward viewer
     :tester ((object)
              (clim:command-enabled 'com-forward clim:*application-frame*))
     :menu   nil)
    (object)
  '())

(clim:define-presentation-method clim:present ((object t)
                                               (type   forward-button)
                                               (stream clim:extended-output-stream)
                                               (view   t)
                                               &key)
  (clim:with-drawing-options (stream :ink (if (forward-possible? object)
                                              clim:+black+
                                              clim:+gray60+))
    (write-string "▶" stream)))

(defun display-history (frame pane)
  (declare (ignore frame))
  (let ((history (model pane)))
    (clim:present history 'back-button :stream pane)
    (write-char #\Space pane)
    (clim:present history 'forward-button :stream pane)))
