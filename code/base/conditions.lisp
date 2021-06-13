(cl:in-package #:dpans-conversion.base)

(define-condition annotation-mixin ()
  ((%annotations :initarg  :annotations
                 :type     list
                 :reader   annotations
                 :initform nil)))

(defmethod print-object :around ((object annotation-mixin) stream)
  (pprint-logical-block (stream (list object))
    (call-next-method)
    (when (and (not *print-readably*) (not *print-escape*) *print-pretty*)
      (a:when-let ((annotations (annotations object)))
        (format stream "~:@_~:@_")
        (text.source-location.print:print-annotations
         stream annotations)))))
