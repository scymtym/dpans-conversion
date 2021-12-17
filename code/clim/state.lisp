(cl:in-package #:dpans-conversion.clim)

;;; Change hook utilities

(defun run-change-hook (object &rest args)
  (let ((hooks (change-hook object)))
    (map nil (lambda (hook)
               (with-simple-restart (continue "Skip hook ~A" hook)
                 (apply hook object args)))
         hooks)))

;;; `history'
;;;
;;; Invariants
;;; * After initialization, there has to be at least one element
;;; * Element referenced by (1- fill-pointer) is the current element

(defclass history ()
  ((%path        :initarg  :path
                 :type     vector
                 :reader   path
                 :initform (make-array 0 :adjustable t :fill-pointer 0))
   (%change-hook :initarg  :change-hook
                 :type     list         ; of functions
                 :accessor change-hook
                 :initform '())))

(defmethod back-possible? ((history history))
  (> (fill-pointer (path history)) 1))

(defmethod forward-possible? ((history history))
  (let ((path (path history)))
    (and (< (fill-pointer path) (array-total-size path))
         (not (null (aref path (fill-pointer path)))))))

(defmethod push-node ((node t) (history history))
  (let ((path (path history)))
    (vector-push-extend node path 1)
    (loop :for i :from (fill-pointer path) :below (array-total-size path)
          :do (setf (aref path i) nil)))
  (run-change-hook history :push-node node))

(defmethod back ((history history))
  (let ((path (path history)))
    (decf (fill-pointer path))
    (let ((node (aref path (1- (fill-pointer path)))))
      (run-change-hook history :back node)
      node)))

(defmethod forward ((history history))
  (let ((path (path history)))
    (incf (fill-pointer path))
    (let ((node (aref path (1- (fill-pointer path)))))
      (run-change-hook history :forward node)
      node)))

;;; `content-state'

(defclass content-state ()
  (;; Data
   (%builder          :accessor builder
                      :initform 'list)
   (%root             :initarg  :root
                      :accessor root) ; TODO will be reader. this is for debugging
   (%node             :initarg  :node
                      :reader   node
                      :writer   (setf %node)
                      :initform nil)
   ;; Display parameters
   (%text-style       :initarg  :text-style
                      :reader   text-style
                      :writer   (setf %text-style)
                      :initform (clim:make-text-style :serif nil :large))
   (%annotations      :reader   annotations
                      :writer   (setf %annotations)
                      :initform '())
   (%highlight-node   :initarg  :highlight-node
                      :type     (or null function)
                      :reader   highlight-node
                      :writer   (setf %highlight-node)
                      :initform nil)
   (%highlight-string :initarg :highlight-string
                      :type    (or null string)
                      :reader  highlight-string
                      :initform nil)
   ;; Change hook
   (%change-hook      :initarg  :change-hook
                      :type     list    ; of function
                      :accessor change-hook
                      :initform '()))
  (:default-initargs
   :root (a:required-argument :root)))

(defmethod (setf node) ((new-value t) (object content-state)
                        &key (run-change-hook t))
  (let ((old-value (node object)))
    (setf (%node object) new-value)
    (when (and run-change-hook (not (eq new-value old-value)))
      (run-change-hook object :node-changed new-value))))

(defmethod (setf text-style) ((new-value t) (object content-state)
                               &key (run-change-hook t))
  (let ((old-value (text-style object)))
    (setf (%text-style object) new-value)
    (when (and run-change-hook (not (climi::text-style-equalp new-value old-value)))
      (run-change-hook object :text-style-changed new-value))))

(defmethod (setf annotations) ((new-value t) (object content-state)
                               &key (run-change-hook t))
  (let ((old-value (node object)))
    (setf (%annotations object) new-value)
    (when (and run-change-hook (not (equal new-value old-value)))
      (run-change-hook object :annotations-changed new-value))))

(defmethod (setf highlight-node) ((new-value t) (object content-state)
                                  &key (run-change-hook t))
  (let ((old-value (highlight-node object)))
    (setf (%highlight-node object) new-value)
    (when (and run-change-hook (not (eq new-value old-value)))
      (run-change-hook object :highlight-changed new-value))))

;;; `observer-mixin'

(defclass observer-mixin ()
  ((%model         :accessor model
                   :initform nil)
   (%hook-function :initarg  :hook-function
                   :reader   hook-function
                   :writer   (setf %hook-function)
                   :initform nil)))

(defmethod shared-initialize :after ((instance   observer-mixin)
                                     (slot-names t)
                                     &key (model nil model-supplied?))
  (unless (hook-function instance)
    (setf (%hook-function instance)
          (lambda (state &rest args)
            (let ((event (make-instance 'state-change-event
                                        :sheet     instance
                                        :arguments (list* state args))))
              (typecase instance
                (clim:application-frame
                 (clim:queue-event
                  (clim:frame-top-level-sheet instance) event))
                (clim:sheet
                 (clim:queue-event instance event)))))))
  (when model-supplied?
    (setf (model instance) model)))

(defmethod (setf model) :around ((new-value content-state) (object observer-mixin))
  (let ((hook-function (hook-function object))
        (old-model     (model object)))
    (when old-model
      (a:removef (change-hook old-model) hook-function))
    (call-next-method)
    (push hook-function (change-hook new-value))))

(defclass state-change-event (climi::standard-event)
  ((%arguments :initarg :arguments
               :reader  arguments)))

;;; `observer-pane'

(defclass observer-pane (observer-mixin) ())

(defmethod clim:note-sheet-adopted :after ((sheet observer-pane))
  (a:when-let ((model (model sheet)))
    (push (hook-function sheet) (change-hook model))))

(defmethod clim:note-sheet-disowned :before ((sheet observer-pane))
  (a:when-let ((model (model sheet)))
    (a:removef (change-hook model) (hook-function sheet))))
