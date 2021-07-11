;;;; See https://developer.mozilla.org/en-US/docs/Web/MathML

(cl:in-package #:dpans-conversion.html)

(define-render (:splice) ; TODO remove after expansion?
  (funcall recurse))

(define-render (:chunk content)
  (cond ((not *math?*)
         (cxml:text content))
        ((or (equal content "{") (equal content "}"))
         (cxml:with-element "mo"
           (cxml:text content)))
        (t
         (cxml:text content))))

(define-render (:block)               ; TODO move to different file
  ;; TODO collect runs of bnf-rule
  (let ((builder (transform:builder transform)))
    (cond (*math?*
           (funcall recurse))
          ((some (lambda (child)
                   (eq (bp:node-kind builder child) :bnf-rule))
                 (bp:node-relation builder '(:element . *) node))
           (cxml:with-element "table"
             (cxml:attribute "class" "bnf")
             (funcall recurse)))
          (t
           (funcall recurse)))))

(define-render (:hbox) ;  :vbox :vtop
  (cond (*math?*
         (funcall recurse :relations '((:element . *))))
        (t
         (funcall recurse :relations '((:element . *))))))

(defun math (style continuation)
  (cxml:with-element "math"
    (cxml:attribute "xmlns" "http://www.w3.org/1998/Math/MathML")
    (ecase style
      (:inline)
      (:block (cxml:attribute "display" "block")))
    (funcall continuation)))

(define-render (:math)
  (math :inline (lambda ()
                  (let ((*math?* t))
                    (funcall recurse)))))

(define-render (:math-display)
  (let* ((builder  (transform:builder transform))
         (elements (bp:node-relation builder '(:element . *) node)))
    (if (and (a:length= 1 elements) ; HACK for Credits section on title page
             (eq (bp:node-kind builder (first elements)) :table))
        (funcall recurse)
        (math :block (lambda ()
                       (let ((*math?* t))
                         (funcall recurse)))))))

(defmethod transform:transform-node :around
    ((transform transform) recurse relation relation-args node
     (kind (eql :other-command-application)) relations
     &key name)
  (cond ((not *math?*)
         (call-next-method))
        ((a:when-let ((primitive (tex:find-primitive name)))
           (when (member :math (tex:modes primitive))
             (cond ((typep primitive 'tex::math-symbol)
                    (let ((string (tex::character primitive)))
                      (cxml:with-element (if (member :relation (tex:tags primitive)) "mo" "mi")
                        (cxml:text string))))
                   ((string= name "sqrt")
                    (cxml:with-element "msqrt"
                      (funcall recurse)))
                   ((member :meta-delimiter (tex:tags primitive))
                    (cxml:with-element "mo"
                      (cxml:attribute "stretchy" "true")
                      (funcall recurse)))
                   (t
                    (cxml:text (format nil "\\~A" name))
                    (funcall recurse :function  (lambda (recurse relation-args relation node kind relations
                                                         &rest initargs)
                                                  (cxml:text "{")
                                                  (apply #'transform:transform-node
                                                         transform recurse relation-args relation node kind relations
                                                         initargs)
                                                  (cxml:text "}"))
                                     :relations '((:argument . *)))))
             t)))
        (t
         (span "error" (let ((*print-level* 2) (*print-circle* t))
                         (format nil "unexpanded non-math macro: ~S" node)))
         #+TODO (call-next-method))))

(defmethod transform:transform-node
    ((transform transform) recurse relation relation-args node
     (kind (eql :other-command-application)) relations
     &key name)
  (span "error" (let ((*print-level* 2) (*print-circle* t))
                  (format nil "unexpanded macro: ~S" node))))
