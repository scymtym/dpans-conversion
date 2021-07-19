(cl:in-package #:dpans-conversion.transform)

(defclass symbol-index (builder-mixin)
  ((%table :reader   table ; TODO could reuse reference table of `build-references'
           :initform (make-hash-table :test #'equal))))

(defmethod transform-node ((transform symbol-index)
                           recurse relation relation-args node kind relations &key)
  (funcall recurse))

;;; Name collection

(defmethod transform-node ((transform symbol-index)
                           recurse relation relation-args node (kind (eql :component)) relations
                           &key ftype)
  (let* ((builder    (builder transform))
         (table      (table transform))
         (name-nodes (bp:node-relation builder '(:name . *) node))
         (namespace  (namespace<-ftype ftype)))
    (map nil (lambda (name-node)
               (multiple-value-bind (name setf?)
                   (evaluate-to-string builder name-node)
                 (setf (getf (gethash name table) namespace)
                       name-node)))
         name-nodes))
  (call-next-method))

(defmethod transform-node ((transform symbol-index)
                           recurse relation relation-args node (kind (eql :index)) relations
                           &key namespace)
  (let* ((builder   (builder transform))
         (table     (table transform))
         (name-node (or (bp:node-relation builder '(:title . 1) node)
                        (bp:node-relation builder '(:name . 1) node)))
         (name      (string-downcase (to-string builder node))))
    (when (member namespace '(:symbol :keyword :package :lambda-list-keyword))
      (setf (getf (gethash name table) namespace) name-node)))
  (call-next-method))

;;; Index generation

(defun symbol-bindings (builder name nodes)
  (let ((namespaces (namespaces **reference-meta-environment**)))
    (loop :with first? = t
          :for namespace :in namespaces
          :for node = (getf nodes namespace)
          :when (and node (not first?))
          :collect (bp:node (builder :chunk :content ", "))
          :when node
            :collect (bp:node (builder :chunk :content (string-downcase namespace)))
            :and :collect (bp:node (builder :chunk :content " "))
            :and :collect (bp:node (builder :reference :name      name
                                                       :namespace namespace))
            :and :do (setf first? nil))))

(defun symbol-list (builder symbols)
  (let ((sorted (sort (copy-seq symbols) #'string-lessp :key #'car)))
    (bp:node (builder :item-list)
      (* (:element . *) (map 'list (lambda (name-and-nodes)
                                     (destructuring-bind (name . nodes) name-and-nodes
                                       (bp:node (builder :list-item)
                                         ; (1 (:key  . *) (bp:node (builder :chunk :content name)))
                                         (* (:body . *) (symbol-bindings builder name nodes)))))
                             sorted)))))

(defun symbol-index-section (builder title symbols)
  (bp:node (builder :section :level 2)
    (1 (:name    . 1) (bp:node (builder :chunk :content title)))
    (1 (:element . *) (symbol-list builder symbols))))

(defun symbol-index-sections (builder symbols)
  (flet ((make-section (title predicate)
           (let ((symbols (remove-if-not
                           (lambda (symbol)
                             (funcall predicate (aref (car symbol) 0)))
                           symbols)))
             (symbol-index-section builder title symbols))))
    (append (map 'list (lambda (letter)
                         (make-section (string letter) (a:curry #'char-equal letter)))
                 "ABCDEFGHIJKLMNOPQRSTUVWXZY")
            (list (make-section "Non-alphabetic" (complement #'alpha-char-p))))))

(defmethod transform-node ((transform symbol-index)
                           recurse relation relation-args
                           node (kind (eql :collection)) relations
                           &rest initargs &key)
  ;; Traverse all nodes to collect issues.
  (funcall recurse)
  ;; Build indices.
  (let* ((builder    (builder transform))
         ; (namespaces (namespaces **meta-environment**)) ; TODO collect in transform
         (entries    (a:hash-table-alist (table transform))))
    ;;
    (let* ((elements     (bp:node-relation builder '(:element . *) node))
           (symbol-index (bp:node (builder :output-file :filename "symbol-index"
                                                        :title    "Symbol Index")
                           (1 (:element . 1) (bp:node (builder :section :level 1)
                                               (1 (:name    . 1) (bp:node (builder :chunk :content "Symbol Index")))
                                               (* (:element . *) (symbol-index-sections builder entries))))))
           (new-elements (append elements (list symbol-index))))
      (bp:node (builder :collection)    ; TODO use reconstitute
        (* (:element . *) new-elements)))))
