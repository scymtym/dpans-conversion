(cl:in-package #:dpans-conversion.transform)

;;; `cleanup-issues'
;;;
;;; * Try to determine the status of the issue. If the status is
;;;   "passed" mark the passed proposal as accepted and other
;;;   proposals as rejected.

(defclass cleanup-issues (default-reconstitute-mixin
                          builder-mixin)
  ((%proposal :accessor proposal
              :initform nil)))

(defmethod transform-node ((transform cleanup-issues) recurse
                           relation relation-args node (kind (eql :proposal)) relations
                           &rest initargs &key name)
  (let* ((builder  (builder transform))
         (proposal (proposal transform))
         (status   (cond ((eq proposal t)
                          :passed)
                         ((eq proposal nil)
                          :unknown)
                         ((string-equal proposal name)
                          :passed)
                         (t
                          :unknown))))
    (apply #'reconstitute builder recurse kind relations
           :status status initargs)))

(defmethod transform-node ((transform cleanup-issues) recurse
                           relation relation-args node (kind (eql :issue)) relations
                           &rest initargs &key)
  (let* ((builder     (builder transform))
         (status-node (find-if (lambda (node)
                                 (let ((name (getf (bp:node-initargs builder node) :name)))
                                   (string-equal name "status")))
                               (bp:node-relation builder '(:section . *) node)))
         (status      (when status-node
                        (to-string builder status-node)))
         (passed?     (when status
                        (or (search "passed" status :test #'char-equal)
                            (search "accepted" status :test #'char-equal))))
         (index       (when status
                        (search #1="proposal " status :test #'char-equal)))
         ;; TODO we should look for :possible-reference nodes instead of this
         (start       (when index
                        (+ index (length #1#))))
         (end         (when start
                        (position-if (a:rcurry #'member '(#\Space #\Tab #\) #\,)) status
                                     :start start)))
         (proposal    (cond ((and start end)
                             (subseq status start end))
                            (passed?
                             t)
                            (t
                             nil))))
    (when (and status-node (not status)) (break "~A" node))
    (setf (proposal transform) proposal)
    (unwind-protect
         (apply #'reconstitute builder recurse kind relations
                :status (if passed? :passed :unknown) initargs)
      (setf (proposal transform) nil)))
  #+no (let ((builder         (builder transform))

             (other-relations (remove '(:element . *) relations :test #'equal))
             (ftype-element   nil)
             (other-elements  '()))
         ;; Find `:element' child which contains the `:ftype' node. This
         ;; can be an ancestor instead of a child due to, for example,
         ;; issue annotations.
         (map nil (lambda (child)
                    (if (find-ancestor-of-kind builder :ftype child)
                        (setf ftype-element child)
                        (push child other-elements)))
              (bp:node-relation builder '(:element . *) node))
         ;; Relate the `:ftype' node via a new `:ftype' relation. Also
         ;; store the ftype as an initarg for convenience.
         (let* ((ftype-node (find-ancestor-of-kind builder :ftype ftype-element))
                (ftype      (node-name ftype-node))
                (new-node   (apply #'%reconstitute builder recurse kind other-relations
                                   :ftype ftype initargs))
                (relations  (list (list '1 '(:ftype   . 1) ftype-element)
                                  (list '* '(:element . *) (nreverse other-elements))))
                (new-node   (bp:add-relations builder new-node relations)))
           (bp:finish-node builder kind new-node))))
