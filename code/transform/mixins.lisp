(cl:in-package #:dpans-conversion.transform)

;;; `builder-mixin'

(defclass builder-mixin ()
  ((%builder :initarg :builder
             :reader  builder))
  (:default-initargs
   :builder (a:required-argument :builder)))

;;; `peeking-mixin'

(defclass peeking-mixin ()
  ())

(defmethod apply-transform ((transform peeking-mixin) (ast t))
  (let ((builder (builder transform)))
    (labels ((peek (builder relation relation-args node)
               (let ((kind (bp:node-kind builder node)))
                 (peek-node transform builder relation relation-args node kind)))
             (visit (recurse relation relation-args node kind relations &rest initargs)
               (apply #'transform-node transform recurse
                      relation relation-args node kind relations
                      initargs)))
      (bp:walk-nodes builder (bp:peeking #'peek #'visit) ast))))

;;; `default-reconstitute-mixin'

(defclass default-reconstitute-mixin ()
  ())

(defun %reconstitute (builder recurse kind relations &rest initargs) ; TODO the second form seems useful but is probably not used consistently
  (let ((node (apply #'bp:make-node builder kind initargs)))
    (bp:add-relations
     builder node
     (map 'list (lambda (relation)
                  (typecase relation
                    ((or symbol (cons symbol atom)) ; NAME or (NAME . CARDINALITY)
                     (multiple-value-bind (relation* cardinality)
                         (bp:normalize-relation relation)
                       (declare (ignore relation*))
                       (let ((right (first (funcall recurse :relations (list relation)))))
                         (list cardinality relation right))))
                    ((cons t (cons t (cons t null))) ; (CARDINALITY NAME RELATED-NODES)
                     relation)))
          relations))))

(defun reconstitute (builder recurse kind relations &rest initargs)
  (let ((node (apply #'%reconstitute builder recurse kind relations initargs)))
    (bp:finish-node builder kind node)))

(defmethod transform-node ((transform default-reconstitute-mixin) recurse
                           relation relation-args node kind relations
                           &rest initargs)
  (let ((builder (builder transform)))
    (apply #'reconstitute builder recurse kind relations initargs)))

;;; `environment-mixin'

(defclass environment-mixin ()
  ((%environment :initarg  :environment
                 :accessor environment))
  (:default-initargs
   :environment (a:required-argument :environment)))

(defmethod depth ((transform environment-mixin))
  (env:depth (environment transform)))

(defmethod push-environment ((transform environment-mixin)
                             (names     sequence)
                             (values    sequence))
  (let ((environment (environment transform)))
    (setf (environment transform)
          (env:augmented-environment environment names values))))

(defmethod pop-environment ((transform environment-mixin))
  (setf (environment transform) (env:parent (environment transform))))

(defmethod call-with-environment ((continuation function)
                                  (transform    environment-mixin)
                                  (names        sequence)
                                  (values       sequence))
  (push-environment transform names values)
  (unwind-protect
       (funcall continuation)
    (pop-environment transform)))

;;; `file-tracking-mixin'

(defclass file-tracking-mixin ()
  ())

;;; Input files

(defmethod current-file ((transform file-tracking-mixin))
  (env:lookup :current-file :traversal (environment transform)))

(defmethod file-stack ((transform file-tracking-mixin))
  (env:lookup :file-stack :traversal (environment transform)
                          :if-does-not-exist '()))

(defmethod include-depth ((transform file-tracking-mixin))
  (length (file-stack transform)))

(defmethod enter ((transform t) (kind (eql :file)) (which t))
  (format t "~V@TEntering file ~S~%" (* 2 (include-depth transform)) which))

(defmethod leave ((transform t) (kind (eql :file)) (which t))
  (format t "~V@TLeaving file ~S~%" (* 2 (include-depth transform)) which))

(defmethod transform-node :around
    ((transform file-tracking-mixin) recurse
     relation relation-args node (kind (eql :file)) relations
     &key filename &allow-other-keys)
  (let* ((old-file-stack (file-stack transform))
         (new-file-stack (list* filename old-file-stack))
         (name           (pathname-name filename)))
    (enter transform :file name)
    (unwind-protect
         (call-with-environment #'call-next-method transform
                                '((:current-file . :traversal)
                                  (:file-stack   . :traversal))
                                (list filename new-file-stack))
      (leave transform :file name))))

;;; Output files

(defclass output-file-tracking-mixin ()
  ())

(defmethod current-output-file ((transform output-file-tracking-mixin))
  (env:lookup :current-output-file :traversal (environment transform)
              :if-does-not-exist nil))

(defmethod output-file-stack ((transform output-file-tracking-mixin))
  (env:lookup :output-file-stack :traversal (environment transform)
              :if-does-not-exist '()))

(defmethod transform-node :around
    ((transform output-file-tracking-mixin) recurse
     relation relation-args node (kind (eql :output-file)) relations
     &key filename &allow-other-keys)
  (let* ((old-file-stack (output-file-stack transform))
         (new-file-stack (list* filename old-file-stack)))
    (call-with-environment #'call-next-method transform
                           '((:current-output-file . :traversal)
                             (:output-file-stack   . :traversal))
                           (list filename new-file-stack))))
