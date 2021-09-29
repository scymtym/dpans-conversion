(cl:in-package #:dpans-conversion.clim)

(defvar *ast*)

(defun perform-search (query frame)
  (let ((builder 'list)
        (result  (make-hash-table :test #'eq))
        (count   0))
    (bp:walk-nodes
     builder
     (lambda (recurse relation relation-args node kind relations
              &rest initargs &key &allow-other-keys)
       (loop :for (key value) :on initargs
             :when (and (stringp value)
                        (search query value))
             :do (incf count)
                 (let ((ancestor (find-ancestor-for-display builder node)))
                   (a:ensure-gethash
                    ancestor result
                    (bp:with-builder (builder)
                      (bp:node* (:list-item)
                        (1 (:element . *) (bp:node* (:reference :name   (node-title ancestor)
                                                                :target ancestor)))))))
                 (let ((*print-circle* t) (*print-level* 3))
                   (print node *trace-output*)))
       (when (< (hash-table-count result) 20)
         (funcall recurse)))
     (tree frame))
    (when (plusp count)
      (setf (node (state frame))
            (bp:with-builder (builder)
              (bp:node* (:item-list)
                (* (:element . *) (a:hash-table-values result))))))))

(clim:define-application-frame viewer ()
  ((%tree   :initarg  :tree
            :reader   tree)
   (%state  :reader   state
            :initform (make-instance 'content-state)))
  (:panes
   (history        history-pane :state  (state clim:*application-frame*)
                                :height 10) ; HACK work around bug
   (breadcrumbs    breadcrumbs-pane :state  (state clim:*application-frame*)
                                    :height 10) ; HACK work around bug
   (query         :text-field           ; :placeholder "query"
                              :min-width 200
                              :value-changed-callback
                  (lambda (gadget value)
                    (when (> (length value) 2)
                      (let ((frame (clim:gadget-client gadget)))
                        (perform-search value frame)))))

   (reviewer-note :toggle-button :label "Reviewer Notes"
                                 :value nil
                                 :value-changed-callback (lambda (gadget value)
                                                           (let* ((frame (clim:gadget-client gadget))
                                                                  (pane  (clim:find-pane-named frame 'content))
                                                                  (state (state pane)))
                                                             (if value
                                                                 (push :reviewer-note (annotations state))
                                                                 (a:removef (annotations state) :reviewer-note)))))
   (editor-note   :toggle-button :label "Editor Notes"
                                 :value nil)
   (issue         :toggle-button :label "Issues"
                                 :value nil)
   (removable     :toggle-button :label "Removable Text"
                                 :value nil)
   (content       content-pane   :state (state clim:*application-frame*))
   (interactor    :interactor    :borders nil))
  (:layouts
   (default
    (clim:spacing (:thickness 8)
      (clim:vertically (:spacing 8)
        (clim:horizontally (:spacing 16)
          history
          (:fill breadcrumbs)
          (clim:horizontally ()
            (clim:labelling (:label      "Q"
                             :text-style (clim:make-text-style nil :bold nil)))
            query))
        (:fill (clim:scrolling () content))
        (clim:horizontally (:spacing 8)
          reviewer-note
          editor-note
          issue
          removable)
                                        ; (1/8   interactor)
        ))))
  (:menu-bar nil)
  (:pointer-documentation t)
  (:update-instances-on-redefinition t))

(define-viewer-command (com-update :keystroke #\Return) ()
  (clim:redisplay-frame-panes clim:*application-frame*))

(define-viewer-command (com-home :keystroke (#\h :control))
    ()
  (let* ((frame   clim:*application-frame*)
         (content (clim:find-pane-named frame 'content)))
    (setf (node (state content)) (tree frame))))

(define-viewer-command (com-mark-for-meditation-session)
    ((node link :gesture (:select
                          :documentation "Mark for meditation session"
                          :pointer-documentation "Mark for meditation session"
                          :priority -1))))


(define-viewer-command (com-praise)
    ((node link :gesture (:select
                          :documentation "Praise the committee for their infinite wisdom"
                          :pointer-documentation "Praise the committee for infinite wisdom"
                          :priority -1))))

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
  (find-ancestor-of-kind builder '(:section :chapter :component :reviewer-note :editor-node)
                         descendent))

(defun node-title (node)
  (or (ignore-errors (transform::node-name* node))
      (ignore-errors (transform::node-name node))))

(define-viewer-command (com-visit)
    ((node link
       :gesture (:select
                 :documentation "Visit node"
                 :pointer-documentation ((object stream)
                                         (destructuring-bind (&key name namespace
                                                              &allow-other-keys)
                                             (bp:node-initargs 'list object)
                                           (format stream "Visit ~(~A~) ~A"
                                                   namespace name))))))
  (let* ((new-node (find-ancestor-for-display 'list node))
         (frame    clim:*application-frame*)
         (content  (clim:find-pane-named frame 'content))
         (title    (or (node-title new-node) "<name>")))
    (setf (clim:frame-pretty-name frame)
          (format nil "~A — ~A" title "Well Specified Common Lisp"))
    (let ((state (state content)))
      (setf (highlight state :run-change-hook nil)
            (if (eq node new-node)
                nil
                (lambda (relation relation-args node* kind relations
                         &rest initargs)
                  (declare (ignore relation relation-args kind relations initargs))
                  (eq node* node))))
      (setf (node state) new-node))))

(defun browse (&key (tree *ast*))
  (clim:find-application-frame 'viewer :tree tree :node tree :create :force))
