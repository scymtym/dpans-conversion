(cl:in-package #:dpans-conversion.transform)

(defclass note-indices (builder-mixin)
  ((%reviewer-notes :reader   reviewer-notes ; TODO could reuse reference table of `build-references'
                    :initform (make-hash-table :test #'equal))
   (%editor-notes   :reader   editor-notes ; TODO could reuse reference table of `build-references'
                    :initform (make-hash-table :test #'equal))))

(defmethod transform-node ((transform note-indices)
                           recurse relation relation-args node kind relations &key)
  (funcall recurse))

;;; Note collection

(flet ((note-key (content) ; TODO this code is repeated in build-references
         (map 'string (lambda (character)
                        (case character
                          (#\Space #\-)
                          (t       (char-downcase character))))
              (subseq content 0 (min (length content) 20)))))

  (defmethod transform-node ((transform note-indices)
                             recurse relation relation-args node (kind (eql :reviewer-note)) relations
                             &key reviewer content)
    (let* ((notes (reviewer-notes transform))
           (key   (cons kind (note-key content)))
           (title (format nil "~@[~A's ~]Reviewer Note ~D"
                          reviewer
                          (1+ (hash-table-count notes)))))
      (setf (gethash key notes) title))
    (funcall recurse))

  (defmethod transform-node ((transform note-indices)
                             recurse relation relation-args node (kind (eql :editor-note)) relations
                             &key editor content)
    (let* ((notes (editor-notes transform))
           (key   (cons kind (note-key content)))
           (title (format nil "~@[~A's ~]Editor Note ~D"
                          editor
                          (1+ (hash-table-count notes)))))
      (setf (gethash key notes) title))
    (funcall recurse)))

;;; Index generation

(defun note-list (builder notes)
  (let ((items '()))
    (maphash
     (lambda (namespace-and-key title)
       (destructuring-bind (namespace . key) namespace-and-key
         (let ((reference (bp:node (builder :unresolved-reference :namespace namespace)
                            (1 (:target . 1) (bp:node (builder :chunk :content key)))
                            (1 (:title  . 1) (bp:node (builder :chunk :content title))))))
           (push (bp:node (builder :list-item)
                   (1 (:body . *) reference))
                 items))))
     notes)
    (bp:node (builder :item-list)
      (* (:element . *) (nreverse items)))))

(defun note-section (builder notes title)
  (let ((note-list (note-list builder notes)))
    (bp:node (builder :section :level 1)
      (1 (:name    . 1) (bp:node (builder :chunk :content title)))
      (1 (:element . *) note-list))))

(defmethod transform-node ((transform note-indices)
                           recurse relation relation-args
                           node (kind (eql :collection)) relations
                           &key)
  ;; Traverse all nodes to collect nodes.
  (funcall recurse)
  ;; Build indices.
  (let* ((builder    (builder transform))
         (elements   (bp:node-relation builder '(:element . *) node))
         (note-index (bp:node (builder :output-file :filename "note-indices"
                                                    :title    "Note Indices")
                       (1 (:element . 1) (bp:node (builder :splice)
                                           (1 (:element . *) (note-section builder
                                                                           (reviewer-notes transform)
                                                                           "Reviewer Note Index"))
                                           (1 (:element . *) (note-section builder
                                                                           (editor-notes transform)
                                                                           "Editor Note Index"))))))
         (new-elements (append elements (list note-index))))
    (bp:node (builder :collection)      ; TODO use reconstitute
      (* (:element . *) new-elements))))
