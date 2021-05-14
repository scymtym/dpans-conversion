(cl:in-package #:dpans-conversion.transform)

;;; `default-reconstitute-mixin'

(defclass default-reconstitute-mixin ()
  ())

(defun reconstitute (builder recurse kind relations &rest initargs)
  (bp:make+finish-node+relations
   builder kind initargs
   (map 'list (lambda (relation)
                (typecase relation
                  ((or symbol (cons symbol atom))
                   (multiple-value-bind (relation* cardinality)
                       (bp:normalize-relation relation)
                     (declare (ignore relation*))
                     (let ((right (first (funcall recurse :relations (list relation)))))
                       (list cardinality relation right))))
                  ((cons t (cons t (cons t null)))
                   relation)))
        relations)))

(defmethod transform-node ((transform default-reconstitute-mixin) recurse
                           relation relation-args node kind relations
                           &rest initargs)
  (let ((builder 'list))
    (apply #'reconstitute builder recurse kind relations initargs)))

;;; `environment-mixin'

(defclass environment-mixin ()
  ((%environment :initarg  :environment
                 :accessor environment)))

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

(defmethod enter ((transform t) (kind (eql :file)) (which t))
  (format t "~V@TEntering file ~S~%" (* 2 (depth transform)) which))

(defmethod leave ((transform t) (kind (eql :file)) (which t))
  (format t "~V@TLeaving file ~S~%" (* 2 (depth transform)) which))

(defmethod transform-node :around
    ((transform file-tracking-mixin) recurse
     relation relation-args node (kind (eql :file)) relations
     &key filename &allow-other-keys)
  (let ((name (pathname-name filename)))
    (enter transform :file name)
    (unwind-protect
         (call-with-environment #'call-next-method transform
                                '((:current-file . :traversal)) (list filename))
      (leave transform :file name))))

(defmethod transform-node :around
    ((transform file-tracking-mixin) recurse
     relation relation-args node (ekind (eql :included-file)) relations
     &key filename &allow-other-keys)
  (let ((name (pathname-name filename)))    ; TODO don't just repeat this code
    (enter transform :file name)
    (unwind-protect
         (call-with-environment #'call-next-method transform
                                '((:current-file . :traversal)) (list filename))
      (leave transform :file name))))
