(cl:in-package #:dpans-conversion.transform)

(defclass build-references (default-reconstitute-mixin)
  ((%environment :reader   environment
                 :initform (make-instance 'env:lexical-environment :parent **meta-environment**))
   (%mode        :accessor mode
                 :initform :record)))

;;; Dispatch

(defmethod transform-node ((transform build-references) recurse
                           relation relation-args node (kind (eql :file)) relations
                           &key &allow-other-keys)
  (call-next-method)
  (setf (mode transform) :link)
  (call-next-method))

(defmethod transform-node ((transform build-references) recurse
                           relation relation-args node kind relations
                           &rest initargs &key &allow-other-keys)
  (case (mode transform)
    (:record
     (with-simple-restart (continue "Do not record ~A node" kind)
       (apply #'record-node transform
              recurse relation relation-args
              node kind relations initargs))
     (funcall recurse))
    (:link
     (with-simple-restart (continue "Do not link ~A node" kind)
       (apply #'link-node transform
              recurse relation relation-args
              node kind relations initargs))
     (call-next-method))))

(defmethod record-node ((transform build-references) recurse
                        relation relation-args node kind relations
                        &key &allow-other-keys))

(defmethod link-node ((transform build-references) recurse
                      relation relation-args node kind relations
                      &key &allow-other-keys))

;;;

(defmethod record-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :chapter)) relations
                        &key &allow-other-keys)
  (let ((builder 'list))
    (let ((id (evaluate-to-string
               builder (bp:node-relation builder '(:name3 . 1) node))))
      (setf (env:lookup id :section (environment transform)) node))))

(macrolet
    ((define (kind)
       `(defmethod record-node ((transform build-references) recurse
                                relation relation-args node (kind (eql ,kind)) relations
                                &key &allow-other-keys)
          (let ((builder 'list))
            (let* ((id-node (find-child-of-kind builder :define-section node))
                   (id      (if id-node
                                (node-name id-node)
                                (remove #\Space (node-name node)))))
              (when (env:lookup id :section (environment transform)
                                   :if-does-not-exist nil)
                (error "Duplicate id ~A" id))
              (setf (env:lookup id :section (environment transform)) node))))))
  (define :section)
  (define :sub-section)
  (define :sub-sub-section)
  (define :sub-sub-sub-section)
  (define :sub-sub-sub-sub-section))

(defmethod record-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :component)) relations
                        &key &allow-other-keys)
  (let ((builder 'list))
    (let* ((names     (bp:node-relation builder '(:name . *) node))
           (ftype     (node-name (find-child-of-kind builder :ftype node)))
           (namespace (namespace<-ftype ftype)))
      (map nil (lambda (name)
                 (let ((name (evaluate-to-string builder name)))
                   (setf (env:lookup name namespace (environment transform)) node)))
           names))))

(defmethod record-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :gentry)) relations
                        &key &allow-other-keys)
  (let ((name (string-downcase (node-name node))))
    (setf (env:lookup name :glossary (environment transform)) node)))

;;;

(defmethod link-node ((transform build-references) recurse
                      relation relation-args node (kind (eql :secref)) relations
                      &key &allow-other-keys)
  (env:lookup (node-name node) :section (environment transform)))

(flet ((link (name namespace transform)
         (let* ((environment (environment transform))
                (name        (string-downcase name))
                (stem        (stemmify name)))
           (if (eq name stem)
               (env:lookup name namespace environment)
               (or (env:lookup name namespace environment
                               :if-does-not-exist nil)
                   (env:lookup stem namespace environment))))))
  
  (defmethod link-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :typeref)) relations
                        &key &allow-other-keys)
    (link (node-name node) :type transform))

  (defmethod link-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :declref)) relations
                        &key &allow-other-keys)
    (link (node-name node) :declaration transform))

  (defmethod link-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :specref)) relations
                        &key &allow-other-keys)
    (link (node-name node) :special-operator transform))

  (defmethod link-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :funref)) relations
                        &key &allow-other-keys)
    (link (node-name node) :function transform))
  
  (defmethod link-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :macref)) relations
                        &key &allow-other-keys)
    (link (node-name node) :macro transform))

  (defmethod link-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :varref)) relations
                        &key &allow-other-keys)
    (link (node-name node) :variable transform))

  (defmethod link-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :conref)) relations
                        &key &allow-other-keys)
    (link (node-name node) :constant transform))

  (defmethod link-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :term)) relations
                        &key &allow-other-keys)
    (link (node-name node) :glossary transform)))
