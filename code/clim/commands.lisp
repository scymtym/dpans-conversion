(cl:in-package #:dpans-conversion.clim)

(clim:define-command-table viewer)

;;;

(defun find-ancestor-of-kind (builder kind descendent)
  (labels ((visit (node)
             (let ((kind* (bp:node-kind builder node))
                   parent)
               (cond ((member kind* (a:ensure-list kind))
                      node)
                     ((setf parent (getf (bp:node-initargs builder node)
                                         :parent))
                      (visit (if (typep parent 'transform::reference) ; TODO should be handled in transform module
                                 (transform::target parent)
                                 parent)))
                     (t
                      descendent)))))
    (visit descendent)))

(defun find-ancestor-for-display (builder descendent)
  (find-ancestor-of-kind builder '(:section :chapter :component
                                   :issue
                                   :reviewer-note :editor-node)
                         descendent))

(defun node-title (node)
  (or (ignore-errors (transform::node-name* node))
      (ignore-errors (transform::node-name node))))

;;; Navigation

(clim:define-command (com-home :command-table viewer
                               :name          t
                               :keystroke     (#\h :control))
    ()
  (let* ((frame    clim:*application-frame*)
         (model    (model frame))
         (history  (history frame))
         (new-node (root model)))
    (setf (node model) new-node)
    (push-node new-node history)))

(defmethod clim:command-enabled ((command-name (eql 'com-home))
                                 (frame        clim:application-frame))
  (let ((model (model frame)))
    (not (eq (node model) (root model)))))

(clim:define-command (com-visit :command-table viewer
                                :name          t)
    ((node link
      :gesture (:select
                :documentation         "Visit node"
                :pointer-documentation ((object stream)
                                        (destructuring-bind (&key name namespace
                                                             &allow-other-keys)
                                            (bp:node-initargs 'list object)
                                          (format stream "Visit ~(~A~) ~A"
                                                  namespace name))))))
  (let* ((new-node (find-ancestor-for-display 'list node))
         (frame    clim:*application-frame*)
         (model    (model frame))
         (history  (history frame)))
    ;; Update content
    (setf (highlight-node model :run-change-hook nil)
          (if (eq node new-node)
              nil
              (lambda (relation relation-args node* kind relations
                       &rest initargs)
                (declare (ignore relation relation-args kind relations initargs))
                (eq node* node))))
    (setf (node model) new-node)
    ;; Update history
    (push-node new-node history)))

(clim:define-command (com-back :command-table viewer
                               :name          t
                               :keystroke     (:left :meta))
    ()
  (let* ((frame   clim:*application-frame*)
         (model   (model frame))
         (history (history frame)))
    (when (back-possible? history)      ; TODO disable command instead
      (setf (node model) (back history)))))

(defmethod clim:command-enabled ((command-name (eql 'com-back))
                                 (frame        clim:application-frame))
  (let* ((frame   clim:*application-frame*)
         (history (history frame)))
    (back-possible? history)))

(clim:define-command (com-forward :command-table viewer
                                  :name          t
                                  :keystroke     (:right :meta))
    ()
  (let* ((frame   clim:*application-frame*)
         (model   (model frame))
         (history (history frame)))
    (when (forward-possible? history)   ; TODO disable command instead
      (setf (node model) (forward history)))))

(defmethod clim:command-enabled ((command-name (eql 'com-forward))
                                 (frame        clim:application-frame))
  (let* ((frame   clim:*application-frame*)
         (history (history frame)))
    (forward-possible? history)))

;;; View

;; TODO macro
;; TODO disable when already min/max
;; TODO update slider
(clim:define-command (com-increase-text-size :command-table viewer
                                             :name          t
                                             :keystroke     (#\+ :control))
    ()
  (let* ((frame      clim:*application-frame*)
         (model      (model frame))
         (text-style (text-style model)))
    (multiple-value-bind (family face size)
        (clim:text-style-components text-style)
      (let ((size (climb:normalize-font-size size)))
        (setf (text-style model) (clim:make-text-style family face (1+ size)))))))

(clim:define-command (com-decrease-text-size :command-table viewer
                                             :name          t
                                             :keystroke     (#\- :control))
    ()
  (let* ((frame      clim:*application-frame*)
         (model      (model frame))
         (text-style (text-style model)))
    (multiple-value-bind (family face size)
        (clim:text-style-components text-style)
      (let ((size (climb:normalize-font-size size)))
        (setf (text-style model) (clim:make-text-style family face (1- size)))))))

;;; Debug-ish commands

(clim:define-command (com-update :command-table viewer
                                 :keystroke     #\Return)
    ()
  (clim:redisplay-frame-panes clim:*application-frame*))

(clim:define-command (com-reload :command-table viewer
                                 :name          t
                                 :keystroke     (#\r :control))
    ()
  (let* ((frame clim:*application-frame*)
         (model (model frame)))
    (setf (root model) dpans-conversion::*document-object-tree*
          (node model) (root model))))

(clim:define-command (com-debug :command-table viewer
                                :name          t
                                :keystroke     (#\i :meta))
    ()
  (let* ((frame clim:*application-frame*)
         (model (model frame))
         (node  (node model)))
    (clouseau:inspect (architecture.builder-protocol.inspection:as-tree
                       node 'list)
                      :new-process t)))

(clim:define-command (com-evaluate :command-table viewer)
    ((listing '(or expression listing)
              :gesture (:select
                        :documentation         "Evaluate code"
                        :pointer-documentation "Evaluate code")))
  (let* ((expression (read-from-string (transform::to-string 'list listing)))
         (output     (make-string-output-stream))
         (result     (let ((*standard-output* output))
                       (multiple-value-list
                        (if t
                            (clim-debugger:with-debugger ()
                              (eval expression))
                            (handler-case
                                (eval expression)
                              (error (condition)
                                condition))))))
         (output     (let ((string (get-output-stream-string output)))
                       (unless (a:emptyp string)
                         (string-trim '(#\Newline) string))))
         (title      (format nil "Evaluation Result for ~A ~A"
                             (lisp-implementation-type)
                             (lisp-implementation-version)))
         (message    (format nil "~S~%~
                                  ~@[~<▷ ~@;~A~:>~%~]~
                                  → ~{~S~^, ~}"
                             expression (when output (list output)) result)))
    (clim:notify-user clim:*application-frame* message
                      :title      title
                      :text-style (clim:make-text-style :fix nil nil))))

;;; Serious commands

(clim:define-command (com-mark-for-meditation-session :command-table viewer)
    ((node link
      :gesture (:select
                :documentation         "Mark for meditation session"
                :pointer-documentation "Mark for meditation session"
                :priority              -1))))


(clim:define-command (com-praise :command-table viewer)
    ((node link
      :gesture (:select
                :documentation         "Praise the committee for their infinite wisdom"
                :pointer-documentation "Praise the committee for infinite wisdom"
                :priority              -1))))

(clim:define-command (com-contemplate :command-table viewer)
    ((node link
      :gesture (:select
                :documentation         "Contemplate Exceptional Situations"
                :pointer-documentation "Contemplate Exceptional Situations"
                :priority              -1))))
