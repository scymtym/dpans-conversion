(cl:in-package #:dpans-conversion.transform)

(defun namespace<-index-namespace (index-namespace)
  (ecase index-namespace
    (:symbol              :symbol)
    (:lambda-list-keyword :lambda-list-keyword)
    (:code                :index/code)
    (:keyword             :keyword)
    (:text                :index/text)
    (:term                :index/term)
    (:constant            :index/constant)
    (:package             :package)))

;;; `build-references' transform
;;;
;;; This transform operates in two stages
;;;
;;; 1. Traverse the tree and for each node which can be the target of a reference
;;;
;;;    a. generate an anchor for the node based on its namespace and
;;;       name (the exact computation of both depend on the kind of
;;;       the node).
;;;
;;;    b. record a mapping entry from the reference key for the node
;;;       to the node itself.
;;;
;;; 2. Traverse the tree again and for each reference-like node, use
;;;    the reference key to look up the target node for that reference
;;;    key and store the target node in the reference-like node.

(defclass build-references (builder-mixin
                            default-reconstitute-mixin
                            reference-updating-mixin
                            environment-mixin)
  ((%stage :accessor stage
           :initform :record)
   (%namespace :accessor namespace)) ; remember component namespace when descending into symbol. TODO remember in environment instead?
  (:default-initargs
   :environment (make-instance 'env:lexical-environment :parent **reference-meta-environment**)))

(defmethod print-object ((object build-references) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (stage object) stream)))

;;; Dispatch

(defmethod apply-transform ((transform build-references) ast)
  ;; Stage 1
  (let ((ast (call-next-method)))
    (update-references transform)
    ;; Reset
    (setf (stage transform) :link)
    (clrhash (references transform))
    ;; Stage 2
    (prog1
        (call-next-method transform ast)
      (update-references transform))))

(defmethod transform-node ((transform build-references) recurse
                           relation relation-args node kind relations
                           &rest initargs &key parent &allow-other-keys)
  (flet ((new-node (new-node)
           (note-new-node transform node new-node)
           new-node))
    (when parent
      (check-type parent reference)
      ; (check-type (target parent) (not null))
      (unless (target parent)
        (break "~A ~A" node parent))
      (register-reference transform parent))
    (case (stage transform)
      (:record
       (new-node (or (with-simple-restart (continue "Do not record ~A node" kind)
                       (apply #'record-node transform
                              recurse relation relation-args
                              node kind relations initargs))
                     (call-next-method))))
      (:link
       (new-node (or (apply #'link-node transform
                            recurse relation relation-args
                            node kind relations initargs)
                     (call-next-method)))))))

(defmethod record-node ((transform build-references) recurse
                        relation relation-args node kind relations
                        &key &allow-other-keys)
  nil)

(defmethod link-node ((transform build-references) recurse
                      relation relation-args node kind relations
                      &key &allow-other-keys)
  nil)

;;; Recording stage

(defun record (name namespace node transform)
  (let ((name (if (stringp name) ; TODO
                  (string-downcase name)
                  name)))
    ;; (format t "Recording ~24A ~A~%" namespace name)
    (let ((environment (environment transform)))
      (a:when-let ((existing (env:lookup name namespace environment :if-does-not-exist nil)))
        #+no (let ((stream *standard-output*))
               (fresh-line stream)
               (pprint-logical-block (stream (list node existing))
                 (text.source-location.print:print-annotations
                  stream
                  (list (base:make-annotation
                         (env:lookup :current-file :traversal environment)
                         (node-bounds (builder transform) node) "new")
                        (base:make-annotation
                         (env:lookup :current-file :traversal environment)
                         (node-bounds (builder transform) existing) "existing")))))
        (cerror "Continue" "Duplicate ~A name ~A" namespace name)
        (return-from record))

      (setf (env:lookup name namespace environment) node))))

(defun record-and-reconstitute (transform recurse kind relations initargs name namespace)
  (let* ((builder  (builder transform))
         (anchor   (format nil "~(~A~)-~{~A~^:~}" namespace (if (consp name)
                                                                (list (car name) (cdr name))
                                                                (list name))))
         (new-node (apply #'reconstitute builder recurse kind relations
                          :namespace  namespace
                          :anchor     anchor
                          :references (make-array 0 :adjustable t :fill-pointer 0)
                          initargs)))
    (record name namespace new-node transform)
    new-node))

(defmethod record-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :chapter)) relations
                        &rest initargs &key)
  (let* ((builder  (builder transform))
         (id1      (evaluate-to-string
                    builder (bp:node-relation builder '(:name2 . 1) node)))
         (id2      (evaluate-to-string
                    builder (bp:node-relation builder '(:name3 . 1) node)))
         (new-node (record-and-reconstitute
                    transform recurse kind relations initargs id1 :section)))
    (record id2 :section new-node transform)
    new-node))

(defmethod record-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :section)) relations
                        &rest initargs &key name label &allow-other-keys)
  (let ((builder (builder transform)))
    (cond (label ; \beginSection with \defineSection produces a label
           (record-and-reconstitute
            transform recurse kind relations initargs label :section))
          (name ; issue sections have a name initarg
           (record-and-reconstitute
            transform recurse kind relations initargs name :section))
          (t
           (let ((id (remove #\Space (node-name node)))) ; TODO should not be needed
             (record-and-reconstitute
              transform recurse kind relations initargs id :section))))))

(defmethod record-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :figure)) relations ; TODO will be wrong once we only have figures
                        &rest initargs &key label &allow-other-keys)
  ;; The caption fallback is used by the table index, not by `:figref'
  ;; references.
  (flet ((caption () ; TODO should there be figures without caption?
           (a:when-let* ((builder      (builder transform))
                         (caption-node (bp:node-relation builder '(:caption . 1) node)))
             (to-string builder caption-node))))
    (a:if-let ((key (or label (caption))))
      (record-and-reconstitute
       transform recurse kind relations initargs key :figure)
      (call-next-method))))

(defmethod record-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :symbol)) relations
                        &rest initargs &key)
  (a:when-let* ((builder   (builder transform))
                (namespace (namespace transform)))
    (multiple-value-bind (name setf?) (evaluate-to-string builder node) ; TODO make a function
      (let ((key (if setf?
                     `(setf ,name)
                     name)))
        (record-and-reconstitute
         transform (lambda (&rest args)
                     (declare (ignore args))
                     (break "should not happen"))
         kind '() initargs key namespace)))))

(defmethod record-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :component)) relations
                        &rest initargs &key ftype)
  (let* ((builder   (builder transform))
         (names     (bp:node-relation builder '(:name . *) node))
         (namespace (namespace<-ftype ftype))
         (names     (prog2
                      (setf (namespace transform) namespace)
                        (first (funcall recurse :relations '((:name . *))))
                      (setf (namespace transform) nil)))
         #+no (names     (map 'list (lambda (name-node)
                                      (multiple-value-bind (name setf?)
                                          (evaluate-to-string builder name-node) ; TODO make a function
                                        (let ((key (if setf?
                                                       `(setf ,name)
                                                       name)))
                                          ;; TODO explain why this not (funcall recurse :relations '((:name . *)))
                                          (note-new-node
                                           transform name-node
                                           (record-and-reconstitute
                                            transform
                                            (lambda (&rest args)
                                              (declare (ignore args))
                                              (break "should not happen"))
                                            (bp:node-kind builder name-node)
                                            '()
                                            (bp:node-initargs builder name-node)
                                            key namespace)))))
                              names)))
    (let* ((other-relations (remove '(:name . *) relations :test #'equal))
           (new-node        (apply #'%reconstitute builder recurse kind other-relations
                                   initargs))
           (new-node        (bp:add-relations builder new-node (list (list '* '(:name . *) names)))))
      (bp:finish-node builder kind new-node))))

(defmethod record-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :gentry)) relations
                        &rest initargs &key)
  (let ((name (string-downcase (node-name node))))
    (record-and-reconstitute
     transform recurse kind relations initargs name :glossary)))

(flet ((record-note (transform recurse kind relations initargs content)
         (let ((name (map 'string (lambda (character)
                                    (case character
                                      (#\Space #\-)
                                      (t       (char-downcase character))))
                          (subseq content 0 (min (length content) 20)))))
           (record-and-reconstitute
            transform recurse kind relations initargs name kind))))

  (defmethod record-node ((transform build-references) recurse
                          relation relation-args node (kind (eql :editor-note)) relations
                          &rest initargs &key content)
    (record-note transform recurse kind relations initargs content))

  (defmethod record-node ((transform build-references) recurse
                          relation relation-args node (kind (eql :reviewer-note)) relations
                          &rest initargs &key content)
    (record-note transform recurse kind relations initargs content)))

(defmethod record-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :issue)) relations
                        &rest initargs &key)
  (let ((name (string-downcase (node-name node))))
    (record-and-reconstitute
     transform recurse kind relations initargs name :issue)))

(defmethod record-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :proposal)) relations
                        &rest initargs &key name issue-name)
  (let ((key (cons (string-downcase issue-name)
                   (string-downcase name))))
    (record-and-reconstitute
     transform recurse kind relations initargs key :issue)))

(defmethod record-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :index)) relations
                        &rest initargs &key namespace)
  (let ((namespace (namespace<-index-namespace namespace))
        (name      (string-downcase (node-name node))))
    (record-and-reconstitute
     transform recurse kind relations initargs name namespace)))

;;; Link stage

(defun reference-target-name (builder node)
  (let ((target-node (bp:node-relation builder '(:target . 1) node)))
    (to-string builder target-node)))

(defun compute-title (transform namespace target-node reference-node)
  (let* ((builder         (builder transform))
         (target-kind     (bp:node-kind builder target-node))
         (target-initargs (bp:node-initargs builder target-node)))
    (apply #'compute-title-using-kind
           transform namespace target-node target-kind reference-node
           target-initargs)))

(defgeneric compute-title-using-kind (transform namespace target-node target-kind reference-node
                                      &key &allow-other-keys))

(defmethod compute-title-using-kind ((transform build-references)
                                     namespace target-node target-kind reference-node
                                     &key)
  (let* ((builder (builder transform))
         (title   (reference-target-name builder reference-node)))
    (bp:node (builder :chunk :content title))))

(defmethod compute-title-using-kind ((transform build-references)
                                     namespace target-node (target-kind (eql :section)) reference-node
                                     &key)
  (let ((builder (builder transform)))
    (bp:node-relation builder '(:name . 1) target-node)))

(defmethod compute-title-using-kind ((transform build-references)
                                     namespace target-node (target-kind (eql :chapter)) reference-node
                                     &key)
  (let ((builder (builder transform)))
    (bp:node-relation builder '(:name . 1) target-node)))

(defmethod compute-title-using-kind ((transform build-references)
                                     namespace target-node (target-kind (eql :gentry)) reference-node
                                     &key)
  (let ((builder (builder transform)))
    (bp:node-relation builder '(:name . 1) target-node)))

(defmethod compute-title-using-kind ((transform build-references)
                                     (namespace (eql :issue)) target-node target-kind reference-node
                                     &key process)
  (let* ((builder    (builder transform))
         (issue-name (reference-target-name builder reference-node)))
    (destructuring-bind (&key proposal explicit? &allow-other-keys)
        (bp:node-initargs builder reference-node)
      (let ((title (format nil "~:[~*~;~@[~A ~]Issue ~]~A~@[:~A~]"
                           explicit? process issue-name proposal)))
        (bp:node (builder :chunk :content title))))))

(labels ((normalize (name)
           (typecase name
             (string (string-downcase name))
             (cons   (cons (normalize (car name)) (normalize (cdr name))))))
         (lookup (name namespace transform)
           (let* ((environment (environment transform))
                  (normalized  (normalize name))
                  (stem        (if (stringp normalized)
                                   (stemmify normalized)
                                   normalized)))
             (or (if (equal normalized stem)
                     (env:lookup normalized namespace environment
                                 :if-does-not-exist nil)
                     (or (env:lookup normalized namespace environment
                                     :if-does-not-exist nil)
                         (env:lookup stem namespace environment
                                     :if-does-not-exist nil)))
                 (restart-case
                     (error "~@<Could not find target for reference ~S in ~
                             namespace ~A.~@:>"
                            name namespace)
                   (continue ()
                     :report (lambda (stream)
                               (format stream "Record a broken link"))
                     nil)))))
         (%link (name namespace transform make-new-node)
           (let* ((builder           (builder transform))
                  (target            (lookup name namespace transform))
                  (registered-target (when target
                                       (make-registered-reference
                                        transform target)))
                  (target-initargs   (when target
                                       (bp:node-initargs builder target)))
                  ;; Generate an anchor for the reference so that the
                  ;; target can produce a list of links to its
                  ;; references if desired.
                  (target-anchor     (when target
                                       (getf target-initargs :anchor)))
                  (references        (when target
                                       (getf target-initargs :references)))
                  (anchor            (when target
                                       (format nil "~A-back-~D"
                                               target-anchor (length references))))
                  ;; Store the target reference in an initarg so that
                  ;; it is never traversed (which could lead to
                  ;; non-termination and could insert the target into
                  ;; the output which is generated for the reference
                  ;; node).
                  (new-node          (funcall make-new-node builder target registered-target anchor)))
             ;; Add the reference to the `:references' of the
             ;; target. This cheats a little by mutating a node (the
             ;; target) after its initial creation.
             (when target
               (vector-push-extend new-node references))
             new-node))
         (link (transform recurse node relations initargs name
                &key (namespace (getf initargs :namespace))
                     title)
           (%link name namespace transform
                  (lambda (builder target-node registered-target anchor)
                    (let ((new-initargs (a:remove-from-plist
                                         initargs :namespace)))
                      (if target-node
                          (let ((new-relations (remove :target relations
                                                       :key #'bp:normalize-relation))
                                (title-node    (or title
                                                   (compute-title transform namespace target-node node))))
                            (apply #'reconstitute builder recurse :reference
                                   (list* (list 1 '(:title . 1) title-node) new-relations) ; TODO back-links
                                   :anchor    anchor
                                   :namespace namespace
                                   :target    registered-target
                                   new-initargs))
                          (apply #'reconstitute builder recurse :unresolved-reference
                                 relations :namespace namespace new-initargs)))))))

  (defmethod link-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :unresolved-reference)) relations
                        &rest initargs)
    (let ((name (reference-target-name (builder transform) node)))
      (link transform recurse node relations initargs name)))

  (defmethod link-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :term)) relations
                        &rest initargs)
    (let* ((builder (builder transform))
           (name    (reference-target-name builder node)))
      (link transform recurse node relations initargs name
            :namespace :glossary
            :title     (bp:node (builder :chunk :content name)))))

  (defmethod link-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :issue-reference)) relations
                        &rest initargs &key proposal)
    (let* ((builder           (builder transform))
           (namespace         :issue)
           (name              (reference-target-name builder node))
           (key               (if proposal
                                  (cons name proposal)
                                  name))
           (target            (lookup key namespace transform))
           (registered-target (when target
                                (make-registered-reference transform target)))
           (title-node        (when target
                                (compute-title transform namespace target node))))
      (if target
          (apply #'reconstitute builder recurse :reference
                 (list* (list 1 '(:title . 1) title-node)
                        (remove :target relations :key #'bp:normalize-relation)) ; TODO back-links
                 :namespace namespace
                 :target    registered-target
                 initargs)
          (apply #'reconstitute builder recurse :unresolved-reference
                 relations :namespace namespace initargs))))

  (defmethod link-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :issue-annotation)) relations
                        &rest initargs &key proposal)
    (let* ((builder            (builder transform))
           (namespace          :issue)
           (target-name        (reference-target-name builder node))
           (key                (if proposal
                                   (cons target-name proposal)
                                   target-name))
           (target             (lookup key namespace transform))
           (registered-target  (when target
                                 (make-registered-reference transform target)))
           (title-node        (when target
                                (compute-title transform namespace target node))))
      (if target
          (apply #'reconstitute builder recurse kind
                 (list* (list 1 '(:title . 1) title-node)
                        (remove :target relations :key #'bp:normalize-relation))
                 :namespace namespace :target registered-target initargs)
          (apply #'reconstitute builder recurse kind relations
                 :namespace namespace :target nil initargs))))

  (defmethod link-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :possible-reference)) relations
                        &rest initargs &key namespace must-resolve?)
    (let* ((environment (environment transform))
           (builder     (builder transform))
           (name        (reference-target-name builder node)))
      (multiple-value-bind (namespaces name*)
          (cond (namespace
                 (values (a:ensure-list namespace) (normalize name)))
                ((a:starts-with #\& name)
                 (values '(:lambda-list-keyword) (normalize (subseq name 1))))
                (t
                 (values (append '(:function :macro :special-operator :proposal)
                                 (set-difference (namespaces environment)
                                                 '(:function :macro :special-operator :proposal
                                                   :issue :glossary))
                                 '(:issue :glossary))
                         (normalize name))))
        (let ((match (some (lambda (namespace)
                             (a:when-let ((target (env:lookup name* namespace environment
                                                              :if-does-not-exist nil)))
                               (cons namespace target)))
                           namespaces)))
          (cond (match
                 (destructuring-bind (namespace . target) match
                   (let ((title-node (or (bp:node-relation builder :title node)
                                         (compute-title transform namespace target node))))
                     (bp:make+finish-node+relations
                      builder :reference (list* :namespace namespace :target target
                                                (bp:node-initargs builder node))
                      (list (list 1 '(:title . 1) title-node))))))
                (must-resolve?
                 (let ((namespace (typecase namespace
                                    (null "?")
                                    (cons (first namespace))
                                    (t    namespace))))
                   (apply #'reconstitute builder recurse :unresolved-reference
                          relations :namespace namespace initargs)))
                (t
                 (bp:node (builder :chunk :content name))))))))

  (defmethod link-node ((transform build-references) recurse
                        relation relation-args node (kind (eql :issue)) relations
                        &rest initargs &key)
    (let* ((builder         (builder transform))
           (old-environment (environment transform))
           (proposals       (remove-if-not (lambda (element)
                                             (eq (bp:node-kind builder element) :proposal))
                                           (bp:node-relation builder '(:section . *) node)))
           (new-environment (env:augmented-environment
                             old-environment
                             (map 'list (lambda (proposal)
                                          (let* ((name (getf (bp:node-initargs builder proposal) :name))
                                                 (name (normalize name)))
                                            (cons name :proposal)))
                                  proposals)
                             proposals)))
      (when (not proposals)
        (break "~A" node))
      (setf (environment transform) new-environment)
      (unwind-protect
           (apply #'reconstitute builder recurse kind relations initargs)
        (setf (environment transform) old-environment)))))
