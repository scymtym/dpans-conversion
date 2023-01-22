;;;;
;;;; In the context of this transformation module, references are node
;;;; initargs (in the `bp:node-initargs' sense) that refer to other
;;;; nodes. In contrast to the strictly tree-shaped structure of nodes
;;;; and relations, references cannot be updated by simply
;;;; reconstituting each nodes by preserving initargs and using the
;;;; possibly transformed children. Transformations that introduce
;;;; references or operate on a tree which contains references must
;;;; cooperate to maintain the validity of references.
;;;;
;;;; This file contains a mechanism for maintaining the validity of
;;;; references across reconstituting transforms. To achieve this,
;;;; initarg values that are references to other nodes are wrapped in
;;;; `reference' instances on creation. Reference-aware transforms
;;;; must build a map from all input nodes to all output nodes and use
;;;; that map to update the target of each reference at the end of the
;;;; transformation.

(cl:in-package #:dpans-conversion.transform)

;;; `reference'

(defclass reference ()
  ((%target :initarg  :target
            :type     (not null)
            :accessor target)
   (%hint   :initarg  :hint
            :reader   hint))
  (:default-initargs
   :target (a:required-argument :target)))

(defun make-reference (target &key hint)
  (check-type target (not null))
  (make-instance 'reference :target target :hint hint))

(defun make-registered-reference (transform target)
  (let ((reference (make-reference target :hint :target)))
    (register-reference transform reference)
    reference))

;;; `reference-updating-mixin'

(defclass reference-updating-mixin ()
  ((%references :reader   references
                :initform (make-hash-table :test #'eq))))

(flet ((ensure-cell  (target references)
         (or (gethash target references)
             (setf (gethash target references)
                   (cons nil '())))))

  (defun register-reference (transform reference)
    (let ((cell (ensure-cell (target reference) (references transform))))
      (push reference (cdr cell))))

  (defun note-new-node (transform old-node new-node)
    (let ((cell (ensure-cell old-node (references transform))))
      (assert (null (car cell)))
      (setf (car cell) new-node))))

(defun update-references (transform)
  (a:maphash-values (lambda (cell)
                      (destructuring-bind (new-node . references) cell
                        (when (and (null new-node) (not (null references)))
                          (let ((*print-circle* t)
                                (*print-level* 3))
                            (format t "in ~S bad ~S ~S -> ~S ~S~%"
                                    (stage transform)
                                    new-node
                                    (hint (first references))
                                    (bp:node-kind 'list (target (first references)))
                                    references)))
                        (mapc (lambda (reference)
                                (setf (target reference) new-node))
                              references)))
                    (references transform)))
