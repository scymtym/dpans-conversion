(cl:in-package #:dpans-conversion.clim)

;;; `content-state'

(defclass content-state ()
  (;; Data
   (%builder     :accessor builder
                 :initform 'list)
   (%node        :reader   node
                 :writer   (setf %node)
                 :initform nil)
   ;; Display parameters
   (%highlight   :initarg  :highlight
                 :type     (or null function)
                 :reader   highlight
                 :writer   (setf %highlight)
                 :initform nil)
   (%annotations :reader   annotations
                 :writer   (setf %annotations)
                 :initform '())
   ;; Change hook
   (%change-hook :initarg  :change-hook
                 :type     list         ; of function
                 :accessor change-hook
                 :initform '())))

(defun run-change-hook (object &rest args)
  (let ((hooks (change-hook object)))
    (map nil (lambda (hook)
               (with-simple-restart (continue "Skip hook ~A" hook)
                 (apply hook object args)))
         hooks)))

(defmethod (setf node) ((new-value t) (object content-state)
                        &key (run-change-hook t))
  (let ((old-value (node object)))
    (setf (%node object) new-value)
    (when (and run-change-hook (not (eq new-value old-value)))
      (run-change-hook object :node-changed new-value))))

(defmethod (setf highlight) ((new-value t) (object content-state)
                             &key (run-change-hook t))
  (let ((old-value (highlight object)))
    (setf (%highlight object) new-value)
    (when (and run-change-hook (not (eq new-value old-value)))
      (run-change-hook object :highlight-changed new-value))))

(defmethod (setf annotations) ((new-value t) (object content-state)
                               &key (run-change-hook t))
  (let ((old-value (node object)))
    (setf (%annotations object) new-value)
    (when (and run-change-hook (not (equal new-value old-value)))
      (run-change-hook object :annotations-changed new-value))))

;;; `observer-mixin'

(defclass observer-mixin ()
  ((%state         :accessor state
                   :initform nil)
   (%hook-function :initarg  :hook-function
                   :reader   hook-function
                   :writer   (setf %hook-function)
                   :initform nil)))

(defmethod shared-initialize :after ((instance   observer-mixin)
                                     (slot-names t)
                                     &key (state nil state-supplied?))
  (unless (hook-function instance)
    (setf (%hook-function instance)
          (lambda (state &rest args)
            (let ((event (make-instance 'state-change-event
                                        :sheet     instance
                                        :arguments (list* state args))))
              (clim:queue-event instance event)))))
  (when state-supplied?
    (setf (state instance) state)))

(defmethod (setf state) :around ((new-value content-state) (object observer-mixin))
  (let ((hook-function (hook-function object))
        (old-state     (state object)))
    (when old-state
      (a:removef (change-hook old-state) hook-function))
    (call-next-method)
    (push hook-function (change-hook new-value))))

(defclass state-change-event (climi::standard-event)
  ((%arguments :initarg :arguments
               :reader  arguments)))
