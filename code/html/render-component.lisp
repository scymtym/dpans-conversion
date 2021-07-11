(cl:in-package #:dpans-conversion.html)

(define-render (:ftype)
  (let ((name (node-name node)))
    (span "ftype" name)))

(define-render (:none)
  (span "none" "None"))

(define-render (:part)
  (let ((name (transform::node-name node)))
    (flet ((do-it ()
             (cxml:with-element "dl" ; TODO one dl for all parts?
               (cxml:with-element "dt"
                 (class-attribute "label")
                 (funcall recurse :relations '((:name . 1))))
               (cxml:with-element "dd"
                 (funcall recurse :relations '((:element . *)))))))
      (if (find-if (a:rcurry #'a:starts-with-subseq name)
                   '("Note" "Example" "Pronunciation" "See Also"))
          (removable-text #'do-it)
          (do-it)))))

(define-render (:component)
  (let* ((builder    (transform:builder transform))
         (names      (bp:node-relation builder '(:name . *) node))
         (anchor     (getf (bp:node-initargs builder (first names)) :anchor)))
    (format t "~V@TGenerating component ~{~A~^, ~}~%"
            (* 2 (transform:depth transform))
            (map 'list (a:curry #'transform::evaluate-to-string builder) names))
    (br) ; TODO
    (div* "component" anchor
          (lambda ()
            (div "header"
                 (lambda ()
                   (span "left"
                         (lambda ()
                           (map nil (lambda (name next-name)
                                      (let ((anchor (getf (bp:node-initargs builder name) :anchor)))
                                        (multiple-value-bind (name setf?)
                                            (transform::evaluate-to-string builder name)
                                          (span* "name" anchor
                                                 (lambda () (render-name name setf?)))
                                          (when next-name
                                            (cxml:text ", ")))))
                                names (append (rest names) '(nil)))))
                   (span "right" (a:curry recurse :relations '((:ftype . 1))))))
            (funcall recurse :relations '((:element . *)))))
    (br))) ; TODO
