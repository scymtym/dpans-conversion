(cl:in-package #:dpans-conversion.transform)

;;; Transform `lower-display-tables'
;;;
;;; This transformation turns applications of the dpANS-specific table
;;; macros \display{two,three,…}, \show{two,three,…} and
;;; \tablefig{two,three,…} which are presented as nodes of kind
;;; `:table' with a `:which' `{:display,:show,:figure}' unitary into
;;; plain `:table' nodes. This transformation basically just entails
;;; wrapping the `:cell' nodes of tables with `:which' `:display' in
;;; `:possible-reference' nodes so that they can be turned into proper
;;; references in later transformations.

(defclass lower-display-tables (default-reconstitute-mixin
                                environment-mixin
                                builder-mixin)
  ((%current-caption :accessor current-caption
                     :initform nil))
  (:default-initargs
   :environment (make-instance 'env:lexical-environment :parent **meta-environment**)))

#+unused (defmethod transform-node ((transform lower-display-tables) recurse
                           relation relation-args node (kind (eql :chunk)) relations
                           &key)
  (let* ((environment (environment transform))
         (context     (env:lookup :display? :traversal environment
                                            :if-does-not-exist nil)))
    (if (typep context '(cons (eql :cell)))
        (let* ((builder   (builder transform))
               (name      (to-string builder node))
               (namespace (cdr context)))
          (bp:node (builder :possible-reference :name          name
                                                :namespace     namespace
                                                :must-resolve? t)))
        (call-next-method))))

(defmethod transform-node ((transform lower-display-tables) recurse
                           relation relation-args node (kind (eql :cell)) relations
                           &key)
  (let* ((environment (environment transform))
         (context     (env:lookup :display? :traversal environment
                                            :if-does-not-exist nil)))
    (if (typep context '(cons (eql :table)))
        ;; This is necessary to obtain a clean name string for cells
        ;; like
        ;;
        ;;   … & string{\tt <=} \cr
        ;;
        ;; .
        (let* ((builder   (builder transform))
               (name      (remove #\Space (to-string builder node)))
               (namespace (cdr context)))
          (bp:node (builder :cell) ; TODO reconstitute builder recurse kind
            (bp:? (:element . *) (unless (a:emptyp name)
                                   (bp:node (builder :possible-reference :name          name
                                                                         :namespace     namespace
                                                                         :must-resolve? t))))))
        #+no (call-with-environment
         #'call-next-method
         transform '((:display? . :traversal)) `((:cell . ,(cdr context))))
        (call-next-method))))

(defmethod transform-node ((transform lower-display-tables) recurse
                           relation relation-args node (kind (eql :table)) relations
                           &rest initargs &key which)
  (ecase which
    (:display
     (let* ((builder   (builder transform))
            (caption   (current-caption transform))
            (namespace (cond ((search "operator" caption :test #'char-equal)
                              '(:function :macro :special-operator))
                             ((and (search "function" caption :test #'char-equal)
                                   (not (search "relat" caption :test #'char-equal)))
                              :function)
                             ((and (search "variable" caption :test #'char-equal)
                                   (not (search "applicable to" caption :test #'char-equal)))
                              :variable)
                             ((and (search "type" caption :test #'char-equal)
                                   (not (search "relating to" caption :test #'char-equal))
                                   (not (search "method combination" caption :test #'char-equal))) ; TODO detect this as :method-combination
                              :type)
                             (t
                              nil))))
       (call-with-environment
        (lambda ()
          (apply #'reconstitute builder recurse :table relations initargs))
        transform '((:display? . :traversal)) `((:table . ,namespace)))))
    ((nil :show :figure)
     (call-next-method))))

;;; Attach captions to figures
;;;
;;; Most applies to figures of the form
;;;
;;;   \boxfig
;;;   …
;;;   \caption{…}
;;;   …
;;;   \endfig

(defmethod transform-node ((transform lower-display-tables) recurse
                           relation relation-args node (kind (eql :caption)) relations
                           &key)
  nil)

(defmethod transform-node ((transform lower-display-tables) recurse
                           relation relation-args node (kind (eql :figure)) relations
                           &rest initargs &key)
  (let* ((builder            (builder transform))
         (caption-child      (bp:node-relation builder '(:caption . 1) node))
         (caption-descendent (when (not caption-child)
                               (let ((caption (find-ancestor-of-kind builder :caption node)))
                                 (first (bp:node-relation builder '(:element . *) caption)))))
         (caption-node       (or caption-child caption-descendent))
         (new-caption        (to-string builder caption-node))
         (old-caption        (current-caption transform)))
    (setf (current-caption transform) new-caption)
    (unwind-protect
         (if caption-descendent
             (let* ((new-node (apply #'%reconstitute builder recurse kind relations
                                     initargs))
                    (new-node (bp:add-relations
                               builder new-node `((1 (:caption . 1) ,caption-descendent)))))
               (bp:finish-node builder kind new-node))
             (call-next-method))
      (setf (current-caption transform) old-caption))))
