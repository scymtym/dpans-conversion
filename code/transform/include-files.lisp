(cl:in-package #:dpans-conversion.transform)

(defclass include-files (builder-mixin
                         environment-mixin
                         file-tracking-mixin
                         default-reconstitute-mixin)
  ((%root-directory :initarg :root-directory
                    :reader  root-directory)))

#+no (defun call-as-include (thunk transform)
  (incf (include-depth transform))
  (unwind-protect
       (funcall thunk)
    (decf (include-depth transform))))

#+no (defmacro as-include ((transform) &body body)
  `(call-as-include (lambda () ,@body) ,transform))

(flet ((include-file (transform filename)
         (with-simple-restart (continue "Skip include ~A" filename)
           (let* ((relative (merge-pathnames filename (current-file transform)))
                  (included (merge-pathnames relative (root-directory transform)))
                  (builder  (builder transform))
                  (tree     (dpans-conversion.parser:parse-tex-file
                             builder included
                             :filename      relative
                             :include-depth (include-depth transform))))
             (format t "include[tc] ~A~%" relative)
             (apply-transform transform tree)
             #+no (apply #'transform-node transform (lambda (&rest args &key (relations (bp:node-relations builder tree)) ; TODO
                                                             &allow-other-keys)
                                                      (break "~A" relations)
                                                      (apply recurse :relations relations args))
                         nil '() tree
                         (bp:node-kind builder tree)
                         (bp:node-relations builder tree)
                         (bp:node-initargs builder tree))
                                        ; (funcall recurse :instead tree)
             ))))

  (defmethod transform-node ((transform include-files) recurse
                             relation relation-args node (kind (eql :input)) relations
                             &key name &allow-other-keys)
    (cond ((or (member name '("setup" "setup-for-toc") :test #'equal) ; TODO proper blacklist
               (a:ends-with-subseq ".fig" name))
           nil)
          ((a:ends-with-subseq ".tc" name)
           (include-file transform (subseq name 0 (- (length name) 3))))
          (t
           (include-file transform name))))

  (defmethod transform-node ((transform include-files) recurse
                             relation relation-args
                             node (kind (eql :other-command-application)) relations
                             &key name &allow-other-keys)
    (if (equal name "includeDictionary")
        (let* ((builder  (builder transform))
               (argument (first (bp:node-relation builder :argument node)))
               (name     (evaluate-to-string builder argument)))
          (include-file transform name))
        node)))
