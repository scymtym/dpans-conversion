(cl:in-package #:dpans-conversion.transform)

(defclass build-references (builder-mixin
                            default-reconstitute-mixin)
  ((%environment :reader   environment
                 :initform (make-instance 'env:lexical-environment :parent **meta-environment**))
   (%stage       :accessor stage
                 :initform :record)))

;;; Dispatch

(defmethod transform-node ((transform build-references) recurse
                           relation relation-args node (kind (eql :file)) relations
                           &key include-depth &allow-other-keys)
  (case include-depth
    (0
     (call-next-method)
     (setf (stage transform) :link)
     (call-next-method))
    (t
     (call-next-method))))

(defmethod transform-node ((transform build-references) recurse
                           relation relation-args node kind relations
                           &rest initargs &key &allow-other-keys)
  (case (stage transform)
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

;;; Recording stage

(defun record (name namespace node transform)
  (format t "Recording ~24A ~A~%" namespace name)
  (let ((environment (environment transform)))
    (when (env:lookup name namespace environment :if-does-not-exist nil)
      (error "Duplicate ~A name ~A" namespace name)) ; TODO annotations
    (setf (env:lookup name namespace environment) node)))

(defmethod record-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :chapter)) relations
                        &key &allow-other-keys)
  (let* ((builder (builder transform))
         (id      (evaluate-to-string
                   builder (bp:node-relation builder '(:name3 . 1) node))))
    (record id :section node transform)))

(macrolet
    ((define (kind)
       `(defmethod record-node ((transform build-references) recurse
                                relation relation-args node (kind (eql ,kind)) relations
                                &key &allow-other-keys)
          (let* ((builder (builder transform))
                 (id-node (find-child-of-kind builder :define-section node))
                 (id      (if id-node
                              (node-name id-node)
                              (remove #\Space (node-name node)))))
            (record id :section node transform)))))
  (define :section)
  (define :sub-section)
  (define :sub-sub-section)
  (define :sub-sub-sub-section)
  (define :sub-sub-sub-sub-section))

(defmethod record-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :component)) relations
                        &key &allow-other-keys)
  (let* ((builder   (builder transform))
         (names     (bp:node-relation builder '(:name . *) node))
         (ftype     (node-name (find-child-of-kind builder :ftype node)))
         (namespace (namespace<-ftype ftype)))
    (map nil (lambda (name)
               (multiple-value-bind (name setf?)
                   (evaluate-to-string builder name) ; TODO make a function
                 (let ((key (if setf?
                                `(setf ,name)
                                name)))
                  (record key namespace node transform))))
         names)))

(defmethod record-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :gentry)) relations
                        &key &allow-other-keys)
  (let ((name (string-downcase (node-name node))))
    (record name :glossary node transform)))

;;; Link stage

(labels ((lookup (name namespace transform)
           (let* ((environment (environment transform))
                  (name        (string-downcase name))
                  (stem        (stemmify name)))
             (if (eq name stem)
                 (env:lookup name namespace environment)
                 (or (env:lookup name namespace environment
                                 :if-does-not-exist nil)
                     (env:lookup stem namespace environment)))))
         (link (name namespace transform)
           (let ((builder (builder transform)))
             (bp:node (builder :reference :kind namespace)
               (bp:? (:target . bp:?) (lookup name namespace transform))))))

  (defmethod link-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :secref)) relations
                        &key &allow-other-keys)
    (link (node-name node) :section transform))

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
