(cl:in-package #:dpans-conversion.clim)

;;; `content-pane'

(defclass content-pane (observer-mixin
                        clim:application-pane)
  ()
  (:default-initargs
   :incremental-redisplay t
   :display-function      'display-content
   :display-time          nil
   :end-of-line-action    :wrap*
   :end-of-page-action    :allow
   ;; :text-style (clim:make-text-style :serif nil 16)
   ))

(defmethod clim:handle-event ((client content-pane) (event state-change-event))
  (destructuring-bind (object event &rest args) (arguments event)
    (declare (ignore object args))
    (let ((node-changed? (eq event :node-changed)))
      (setf (clim:pane-needs-redisplay client)
            (if node-changed? t :no-clear))
      (clim:redisplay-frame-pane (clim:pane-frame client) client)
      (when node-changed?
        (clim:scroll-extent client 0 0)))))

(defun display-tree (builder tree stream &key (annotations '())
                                              (highlight   nil))
  (let ((transform (make-instance 'display :builder     builder
                                           :annotations annotations
                                           :highlight   highlight
                                           :stream      stream
                                           :depth       2)))
    (bp:walk-nodes
     builder
     (lambda (recurse relation relation-args node kind relations
              &rest initargs &key &allow-other-keys)
       (apply #'transform:transform-node
              transform recurse relation relation-args node kind relations
              initargs))
     tree)))

(defun display-content (frame pane)
  (declare (ignore frame))
  (clim:updating-output (pane)
    (clime:with-temporary-margins (pane :top   '(:relative 20)
                                        :left  '(:relative 20)
                                        :right '(:relative 40)) ; TODO why does (:relative 20) not work?
      (clim:with-bounding-rectangle* (x1 y1) (clime:stream-page-region pane)
        (setf (clim:stream-cursor-position pane) (values x1 y1))
        (let ((state (state pane)))
          (a:if-let ((tree (node state)))
            (let ((builder     (builder state))
                  (annotations (annotations state))
                  (highlight   (highlight state)))
              (display-tree builder tree pane :annotations annotations
                                              :highlight   highlight))
            (clim:with-drawing-options (pane :text-face :italic :ink clim:+gray30+)
              (write-string "No content selected" pane))))))))

;;; "Link preview" via presentation highlighting

(flet ((make-highlight (record stream)
         (clim:with-bounding-rectangle* (:center-x x :y2 y) record
           (a:when-let* ((builder   'list)
                         (node      (clim:presentation-object record))
                         (new-node  (find-ancestor-of-kind builder '(:component :section :gentry :reviewer-note :editor-note) node))) ; TODO why would this be null?
             (labels ((render ()
                        (clime:with-temporary-margins (stream :left  '(:absolute 0)
                                                              :right `(:absolute ,(* .9 700)))
                          (display-tree 'list new-node stream
                                        :highlight (if (eq node new-node) nil node))))
                      (maybe-border ()
                        (if (eq (bp:node-kind builder new-node) :component)
                            (render)
                            (clim:surrounding-output-with-border
                                (stream :background clim:+white+)
                              (render)))))
               (let ((highlight (clim:with-output-to-output-record (stream)
                                  (maybe-border))))
                 (clim:with-bounding-rectangle* (:width width) highlight
                   (setf (clim:output-record-position highlight)
                         (values (max 20 (- x (/ width 2) ))
                                 (+ y 10)))
                   highlight)))))))

  (clim:define-presentation-method clim:highlight-presentation
      ((type    link)
       (record  t)
       (stream  content-pane)
       (state   (eql :highlight)))
    (a:when-let ((highlight (make-highlight record stream)))
      (clim:replay highlight stream)))

  (clim:define-presentation-method clim:highlight-presentation
      ((type    link)
       (record  t)
       (stream  content-pane)
       (state   (eql :unhighlight)))
    (a:when-let ((highlight (make-highlight record stream)))
      (clim:repaint-sheet stream (clim:bounding-rectangle highlight)))))
