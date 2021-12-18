(cl:in-package #:dpans-conversion.clim)

;;; `breadcrumbs-pane'

(defclass breadcrumbs-pane (observer-mixin
                            clim:application-pane)
  ()
  (:default-initargs
   :display-function   'display-breadcrumbs
   :end-of-line-action :wrap*
   :end-of-page-action :allow
   :text-style         (clim:make-text-style nil :bold nil)
   :foreground         clim:+dark-blue+))

(defmethod clim:note-sheet-adopted :after ((sheet breadcrumbs-pane))
  (setf (clim:pane-background sheet)
        (clim:pane-background (clim:sheet-parent sheet))))

(defmethod clim:handle-event ((client breadcrumbs-pane)
                              (event  state-change-event))
  (destructuring-bind (object event &rest args) (arguments event)
    (declare (ignore object event args))
    (clim:redisplay-frame-pane (clim:pane-frame client) client :force-p t)))

(defun display-breadcrumbs (frame pane)
  (let* ((model     (model pane))
         (root      (root model))
         (node      (node model))
         (ancestors '()))
    ;; Build ancestor path.
    (labels ((visit (ancestor)
               (let ((ancestor (if (typep ancestor 'transform::reference)
                                   (transform::target ancestor)
                                   ancestor)))
                 (a:when-let ((title (node-title ancestor)))
                   (push (cons ancestor title) ancestors))
                 (a:when-let ((parent (getf (bp:node-initargs 'list ancestor) :parent)))
                   (visit parent)))))
      (visit node))
    ;; Render the path.
    (with-output-as-link (pane root :current-node node)
      (write-string "⌂" pane))
    (loop :for (ancestor . title) :in ancestors
          :for first? = t :then nil
          :do (write-string " » " pane)
          :do (with-output-as-link (pane ancestor :current-node node)
                (write-string title pane)))))
