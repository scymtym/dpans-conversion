(cl:in-package #:dpans-conversion.clim)

(defvar *ast*)

(defun perform-search (query frame)
  (let* ((model   (model frame))
         (builder (builder model))
         (result  (make-hash-table :test #'eq))
         (count   0))
    (bp:walk-nodes
     builder
     (lambda (recurse relation relation-args node kind relations
              &rest initargs &key &allow-other-keys)
       (declare (ignore relation relation-args kind relations))
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
                                                                :target ancestor))))))))
       (when (< (hash-table-count result) 20)
         (funcall recurse)))
     (root model))
    (setf (node (model frame))
          (bp:with-builder (builder)
            (if (plusp count)
                (bp:node* (:item-list)
                  (* (:element . *) (a:hash-table-values result)))
                (bp:node* (:italic)
                  (1 (:element . *) (bp:node* (:chunk :content "No matches")))))))))

(defun make-annotation-button (label annotation)
  (flet ((callback (gadget value)
           (let* ((frame (clim:gadget-client gadget))
                  (model (model frame)))
             (if value
                 (push annotation (annotations model))
                 (a:removef (annotations model) annotation)))))
   (clim:make-pane :toggle-button :label                  label
                                  :value                  nil
                                  :value-changed-callback #'callback)))

(clim:define-application-frame viewer (observer-mixin
                                       clim:standard-application-frame)
  ((%history :reader   history
             :initform (make-instance 'history)))
  (:panes
   (history        history-pane :model  (history clim:*application-frame*)
                                :height 10) ; HACK work around bug
   (breadcrumbs    breadcrumbs-pane :model  (model clim:*application-frame*)
                                    :height 10) ; HACK work around bug
   (query         :text-field           ; :placeholder "query"
                              :min-width 200
                              :value-changed-callback
                  (lambda (gadget value)
                    (when (> (length value) 2)
                      (let ((frame (clim:gadget-client gadget)))
                        (perform-search value frame)))))

   (reviewer-note (make-annotation-button "Reviewer Notes" :reviewer-note))
   (editor-note   (make-annotation-button "Editor Notes"   :editor-note))
   (issue         (make-annotation-button "Issues"         :issue-annotation))
   (removable     (make-annotation-button "Removable Text" :removable-Text))
   (content       content-pane   :model (model clim:*application-frame*))
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

(defmethod shared-initialize :around ((instance viewer) (slot-names t)
                                      &rest args
                                      &key (root nil root-supplied?)
                                           (node nil node-supplied?))
  (if root-supplied?
      (apply #'call-next-method instance slot-names
             :model (make-instance 'content-state
                                   :root root
                                   :node (if node-supplied? node root))
             (a:remove-from-plist args :root :node))
      (call-next-method)))

(defmethod clim:handle-event ((client viewer) (event state-change-event))
  (destructuring-bind (object event &rest args) (arguments event)
    (declare (ignore object))
    (when (eq event :node-changed)
      (let* ((new-node (first args))
             (title    (or (node-title new-node) "<name>")))
        (setf (clim:frame-pretty-name client)
              (format nil "~A — ~A" title "Well Specified Common Lisp"))))))

(define-viewer-command (com-update :keystroke #\Return) ()
  (clim:redisplay-frame-panes clim:*application-frame*))

(define-viewer-command (com-home :keystroke (#\h :control))
    ()
  (let* ((frame   clim:*application-frame*)
         (model  (model frame)))
    (setf (node model) (root model))))

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

(define-viewer-command (com-contemplate)
    ((node link :gesture (:select
                          :documentation "Contemplate Exceptional Situations"
                          :pointer-documentation "Contemplate Exceptional Situations"
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
  (find-ancestor-of-kind builder '(:section :chapter :component
                                   :issue
                                   :reviewer-note :editor-node)
                         descendent))

(defun node-title (node)
  (or (ignore-errors (transform::node-name* node))
      (ignore-errors (transform::node-name node))))

(define-viewer-command (com-visit)
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

(define-viewer-command (com-back :name      t
                                 :keystroke (:left :meta))
    ()
  (let* ((frame   clim:*application-frame*)
         (model   (model frame))
         (history (history frame)))
    (when (back-possible? history)      ; TODO disable command instead
      (setf (node model) (back history)))))

(define-viewer-command (com-forward :name      t
                                    :keystroke (:right :meta))
    ()
  (let* ((frame   clim:*application-frame*)
         (model   (model frame))
         (history (history frame)))
    (when (forward-possible? history) ; TODO disable command instead
      (setf (node model) (forward history)))))

(define-viewer-command (com-debug :name      t
                                  :keystroke (#\i :meta))
    ()
  (let* ((frame clim:*application-frame*)
         (model (model frame))
         (node  (node model)))
    (clouseau:inspect (architecture.builder-protocol.inspection:as-tree
                       node 'list)
                      :new-process t)))

(define-viewer-command (com-reload :name      t
                                   :keystroke (#\r :control))
    ()
  (let* ((frame clim:*application-frame*)
         (model (model frame)))
    (setf (root model) dpans-conversion::*document-object-tree*
          (node model) (root model))))

(defun browse (&key (document-object-tree dpans-conversion::*document-object-tree*))
  (clim:find-application-frame 'viewer :root   document-object-tree
                                       :create :force))
