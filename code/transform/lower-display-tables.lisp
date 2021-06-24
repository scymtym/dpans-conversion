(cl:in-package #:dpans-conversion.transform)

;;; Transform `lower-display-tables'
;;;
;;; This transformation turns applications of the dpANS-specific table
;;; macros \display{two,three,…}, \show{two,three,…} and
;;; \tablefig{two,three,…} which are presented as nodes of kind
;;; `:table' with a `:which' `{:display,:show,:figure}' into plain
;;; `:table' nodes. This transformation basically just entails
;;; wrapping the `:cell' nodes of tables with `:which' `:display' in
;;; `:possible-reference' nodes so that they can be turned into proper
;;; references in later transformations.

(defclass lower-display-tables (default-reconstitute-mixin
                                environment-mixin
                                builder-mixin)
  ()
  (:default-initargs
   :environment (make-instance 'env:lexical-environment :parent **meta-environment**)))

(defmethod transform-node ((transform lower-display-tables) recurse
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
        (call-with-environment
         #'call-next-method
         transform '((:display? . :traversal)) `((:cell . ,(cdr context))))
        (call-next-method))))

(defmethod transform-node ((transform lower-display-tables) recurse
                           relation relation-args node (kind (eql :table)) relations
                           &rest initargs &key which)
  (ecase which
    (:display
     (let* ((builder   (builder transform))
            (caption   (to-string builder (bp:node-relation
                                           builder '(:caption . 1) node)))
            (namespace (cond ((search "operator" caption :test #'char-equal)
                              '(:function :macro :special-operator))
                             ((search "function" caption :test #'char-equal)
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
