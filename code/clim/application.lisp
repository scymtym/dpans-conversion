(cl:in-package #:dpans-conversion.clim)

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
    (let ((value (find annotation (annotations (model clim:*application-frame*)))))
      (clim:make-pane :toggle-button :label                  label
                                     :value                  value
                                     :value-changed-callback #'callback))))

(flet ((update-text-style (partial-text-style gadget)
         (let ((state (model (clim:gadget-client gadget))))
           (setf (text-style state) (print (clim:merge-text-styles
                                            partial-text-style (text-style state))
                                           *trace-output*)))))

  (defun make-text-family-option-pane ()
    (flet ((callback  (gadget value)
             (update-text-style (clim:make-text-style value nil nil) gadget)))
      (let* ((model (model clim:*application-frame*))
             (value (clim:text-style-family (text-style model))))
        (clim:make-pane :option :items                  '(:sans-serif :serif :fix)
                                :value                  value
                                :name-key               #'string-capitalize
                                :value-changed-callback #'callback))))

  (defun make-text-size-option-pane ()
    (flet ((font-size (text-style)
             (climb:normalize-font-size
              (clim:text-style-size text-style)))
           (callback (gadget value)
             (let ((text-style (clim:make-text-style nil nil value)))
               (update-text-style text-style gadget))))
      (let* ((model  (model clim:*application-frame*))
             (value  (font-size (text-style model)))
             (slider (clim:make-pane :slider
                                     :min-value              8
                                     :max-value              30
                                     :value                  value
                                     :number-of-quanta       (1+ (- 30 8))
                                     :orientation            :horizontal
                                     :value-changed-callback #'callback)))
        (push (lambda (model event new-value)
                (declare (ignore model))
                (when (eq event :text-style-changed)
                  (setf (clim:gadget-value slider :invoke-callback nil)
                        (font-size new-value))))
              (change-hook model))
        slider))))

(clim:define-application-frame viewer (observer-mixin
                                       clim:standard-application-frame)
  ((%history :reader   history
             :initform (make-instance 'history)))
  (:panes
   (history        history-pane     :model  (history clim:*application-frame*)
                                    :height 10) ; HACK work around bug
   (breadcrumbs    breadcrumbs-pane :model  (model clim:*application-frame*)
                                    :height 10) ; HACK work around bug
   (query         :text-field                   ; :placeholder "query"
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
   (interactor    :interactor    :borders nil)

   (settings        (clim:vertically (:spacing 8)
                      (clim:labelling (:label "Font")
                        (clim:tabling (:spacing 8)
                          (list
                           (clim:labelling (:label "Family"))
                           (make-text-family-option-pane))
                          (list
                           (clim:labelling (:label "Size"))
                           (make-text-size-option-pane))))
                      (clim:labelling (:label "Annotations")
                        (clim:vertically (:spacing 8)
                          reviewer-note
                          editor-note
                          issue
                          removable))
                      clim:+fill+))
   (toggle-settings :push-button :label     "C"
                                 :y-spacing 0
                                 :activate-callback
                    (lambda (gadget)
                      (let* ((frame     (clim:gadget-client gadget))
                             (container (clim:find-pane-named frame 'container)))
                        (if (member settings (clim:sheet-children container))
                            (clim:sheet-disown-child container settings)
                            (clim:sheet-adopt-child container settings))))))
  (:layouts
   (default
    (clim:spacing (:thickness 8)
      (clim:vertically (:spacing 4)
        (clim:horizontally (:name 'container :spacing 8)
          (clim:+fill+ (clim:vertically (:spacing 8)
                         (clim:horizontally (:spacing 16)
                           history
                           (:fill breadcrumbs)
                           (clim:horizontally ()
                             (clim:labelling (:label      "Q"
                                              :text-style (clim:make-text-style nil :bold nil)))
                             query)
                           toggle-settings)
                         (clim:+fill+ (clim:scrolling () content))))
          settings)
        (clim:make-pane 'clime:box-adjuster-gadget)
        interactor))))
  (:menu-bar nil)
  (:pointer-documentation t)
  (:command-table (viewer))
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

; (defmethod clim:frame-standard-output ((frame )))

(defmethod clim:handle-event ((client viewer) (event state-change-event))
  (destructuring-bind (object event &rest args) (arguments event)
    (declare (ignore object))
    (when (eq event :node-changed)
      (let* ((new-node (first args))
             (title    (or (node-title new-node) "<name>")))
        (setf (clim:frame-pretty-name client)
              (format nil "~A — ~A" title "Well Specified Common Lisp"))))))

(defun browse (&key (document-object-tree dpans-conversion::*document-object-tree*))
  (clim:find-application-frame 'viewer :root   document-object-tree
                                       :create :force))
