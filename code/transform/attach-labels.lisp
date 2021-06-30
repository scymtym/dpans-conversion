(cl:in-package #:dpans-conversion.transform)

(defclass attach-labels (default-reconstitute-mixin
                         builder-mixin)
  ((%current-figure :accessor current-figure
                    :initform nil)))

;;; Tables labels

(defmethod transform-node ((transform attach-labels) recurse
                           relation relation-args node (kind (eql :define-figure)) relations
                           &key)
  (let ((name (node-name node)))
    (setf (current-figure transform) name))
  nil)

(flet ((attach-label (label transform recurse kind relations initargs)
         (let ((builder (builder transform)))
           (setf (current-figure transform) nil)
           (apply #'reconstitute builder recurse kind relations
                  :label label initargs))))

  (defmethod transform-node ((transform attach-labels) recurse ; TODO turn all tables into :figure?
                             relation relation-args node (kind (eql :table)) relations
                             &rest initargs &key)
    (a:if-let ((label (current-figure transform)))
      (break "should not happen")
      ; (attach-label label transform recurse kind relations initargs)
      (call-next-method)))

  (defmethod transform-node ((transform attach-labels) recurse
                             relation relation-args node (kind (eql :figure)) relations
                             &rest initargs &key)
    (a:if-let ((label (current-figure transform)))
      (attach-label label transform recurse kind relations initargs)
      (call-next-method))))

;;; Section labels

(defmethod transform-node ((transform attach-labels) recurse
                           relation relation-args node (kind (eql :define-section)) relations
                           &key)
  nil)

(defmethod transform-node ((transform attach-labels) recurse
                           relation relation-args node (kind (eql :section)) relations
                           &rest initargs &key)
  (let* ((builder    (builder transform))
         (label-node (block nil
                       (bp:walk-nodes
                        builder
                        (lambda (recurse relation relation-args node* kind relations
                                 &key &allow-other-keys)
                          (declare (ignore relation relation-args relations))
                          (case kind
                            (:define-section
                             (return node*))
                            (:section
                             (when (eq node* node)
                               (funcall recurse)))
                            (t
                             (funcall recurse))))
                        node)
                       nil))
         (label      (when label-node
                       (node-name label-node))))
    (if label
        (apply #'reconstitute builder recurse kind relations
               :label label initargs)
        (call-next-method))))
