(cl:in-package #:dpans-conversion.html)

(define-render (:ftype)
  (let ((name (node-name node)))
    (span "ftype" (lambda () (cxml:text name)))))

(define-render (:none)
  (span "none" (lambda () (cxml:text "None"))))

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

(define-render (:component ftype)
  (let* ((builder    (transform:builder transform))
         (names      (bp:node-relation builder '(:name . *) node))
         ; (ftype-tree (bp:node-relation builder '(:ftype . 1) node))
         ; (ftype      (node-name (transform::find-ancestor-of-kind builder :ftype ftype-tree)))
         (namespace  (dpans-conversion.transform::namespace<-ftype ftype)))
    (format t "~V@TGenerating component ~{~A~^, ~}~%"
            (* 2 (transform:depth transform))
            (map 'list (a:curry #'transform::evaluate-to-string builder) names))
    (br)
    (div "component"
         (lambda ()
           (div "header"
                (lambda ()
                  (span "left"
                        (lambda ()
                          (map nil (lambda (name next-name)
                                     (multiple-value-bind (name setf?)
                                         (transform::evaluate-to-string builder name)
                                       (cxml:with-element "a"
                                         (cxml:attribute "id" (format nil "~(~A~)-~A"
                                                                      namespace
                                                                      name))
                                         (cxml:text " ")) ; HACK
                                       (render-name name setf?)
                                       (when next-name
                                         (cxml:text ", "))))
                               names (append (rest names) '(nil)))))
                  (span "right" (a:curry recurse :relations '((:ftype . 1))))))
           (funcall recurse :relations '((:element . *)))))
    (br)))
