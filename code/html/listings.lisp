(cl:in-package #:dpans-conversion.html)

;;; `read-client'

(defclass read-client (eclector.examples.highlight::highlight-client)
  ())

(defclass tex-macro (eclector.examples.highlight.cst::inner-node)
  ((%name      :initarg  :name
               :reader   eclector.examples.highlight.cst:name)
   (%arguments :initarg  :arguments
               :reader   arguments
               :initform '())))

(defun read-backslash (stream)
  (loop :with readtable   = eclector.reader:*readtable*
        :with token       = (make-array 10 :element-type 'character
                                           :adjustable   t
                                           :fill-pointer 0)
        :with name        = (make-array 2 :element-type 'character
                                          :adjustable   t
                                          :fill-pointer 0)
        :with argument    = nil
        :with arguments   = '()
        :with brace-level = 0
        :for char = (read-char stream nil nil t)
        :for type = (eclector.readtable:syntax-type readtable char)
        :do (flet ((collect (for-tex)
                     (vector-push-extend char token)
                     (when for-tex
                       (vector-push-extend char (or argument name))))
                   (done ()
                     (unread-char char stream)
                     (loop-finish)))
              (case char
                ((nil)
                 (loop-finish))
                ((#\Space #\Tab #\Newline #\,)
                 (if (plusp brace-level)
                     (collect t)
                     (done)))
                (#\{
                 (incf brace-level)
                 (setf argument (make-array 10 :element-type 'character
                                               :adjustable   t
                                               :fill-pointer 0))
                 (collect nil))
                (#\}
                 (push argument arguments)
                 (setf argument nil)
                 (decf brace-level)
                 (collect nil))
                (t
                 (if (and (eq type :terminating-macro)
                          (zerop brace-level))
                     (done)
                     (collect t)))))
        :finally (return
                   (if (member name '("EV" "OV" "NV" "EQ" "OUT" "IN" "term")
                               :test #'equal)
                       (make-instance 'tex-macro :name      name
                                                 :arguments arguments)
                       (eclector.reader:interpret-token
                        eclector.reader:*client* stream (coerce token 'simple-string) '())))))

(let ((eclector.base:*client* (make-instance 'read-client :current-package (find-package '#:cl-user))))
  (eclector.reader:read-from-string ", \\terma{true}"))

(let ((eclector.base:*client* (make-instance 'read-client :current-package (find-package '#:cl-user))))
  (eclector.reader:read-from-string "\\EV 9 \\n"))

(defun read-unreadable (stream)
  (loop :for char = (read-char stream)
        :until (char= char #\>)
        :collect char))

(defvar *extended-readtable*
  (let ((readtable (eclector.readtable:copy-readtable eclector.reader:*readtable*)))
    (eclector.readtable:set-macro-character
     readtable #\\ (lambda (stream char)
                     (declare (ignore char))
                     (read-backslash stream))
     t)
    (eclector.readtable:set-dispatch-macro-character
     readtable #\# #\< (lambda (stream char sub-char)
                         (declare (ignore char sub-char))
                         (read-unreadable stream)))
    readtable))

(defmethod eclector.reader:call-as-top-level-read
    ((client                read-client)
     (thunk                 t)
     (input-stream          t)
     (eof-error-p           t)
     (eof-value             t)
     (preserve-whitespace-p t))
  (let ((eclector.reader:*readtable* *extended-readtable*))
    (call-next-method)))

(defmethod eclector.reader:find-character ((client read-client) (name t))
  (call-next-method))

(defmethod eclector.reader:interpret-token
    ((client        read-client)
     (input-stream  t)
     (token         t)
     (escape-ranges t))
  (if (string= token "...")
      (eclector.reader:interpret-symbol-token client input-stream token nil nil)
      (call-next-method)))

;; TODO read-time-evaluation

;;; `highlight-client'

(defclass highlight-client (eclector.examples.highlight.render::link-mixin
                            eclector.examples.highlight.render::html-fragment-mixin
                            eclector.examples.highlight.render::html-client)
  ())

(defmethod eclector.examples.highlight.render:enter-node
    ((client highlight-client) (node tex-macro)))

(defmethod eclector.examples.highlight.render:leave-node
    ((client highlight-client) (node tex-macro)))

(defmethod eclector.examples.highlight.render:write-character
    ((client    highlight-client)
     (position  t)
     (character t)
     (node      tex-macro))
  (when (eql position (eclector.examples.highlight.cst:start node))
    (let* ((name   (eclector.examples.highlight.cst:name node))
           (stream (eclector.examples.highlight.render:stream client)))
      (a:switch (name :test #'string=)
        ("EV"   (write-string "→" stream))
        ("OV"   (write-string "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mover><mo>→</mo><mo>or</mo></mover></math>" stream))
        ("NV"   (write-string "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mover><mo>→</mo><mo>not</mo></mover></math>" stream))
        ("EQ"   (write-string "≡" stream))
        ("OUT"  (write-string "▷" stream))
        ("IN"   (write-string "<span class=\"syntax-user-input\">" stream)
                (write-string (first (arguments node)) stream)
                (write-string "</span>" stream))
        ("term" (write-string (first (arguments node)) stream)) ; TODO
        (t      ))                                              ; TODO
      )))

(defmethod eclector.examples.highlight.render:write-character
    ((client    highlight-client)
     (position  t)
     (character t)
     (node      eclector.examples.highlight.cst:interned-symbol-node))
  (let ((name (eclector.examples.highlight.cst:name node)))
    (cond ((not (member name '("...") :test #'equal))
           (call-next-method))
          ((eql position (eclector.examples.highlight.cst:start node))
           (let ((symbol (a:eswitch (name :test #'string=)
                           ("..." "…"))))
             (write-string symbol (eclector.examples.highlight.render:stream client))))
          (t))))

#+later (defmethod eclector.examples.highlight::url
            ((client highlight-client)
             (node   eclector.examples.highlight.cst:standard-symbol-node))
          (-url))

;;;

(defun remove-tex-escapes (string)
  (loop :with result = string
        :for index = (or (search "\\\\" result)
                         (search "\\#" result))
        :while index
        :do (setf result (concatenate 'string
                                      (subseq result 0 index)
                                      (subseq result (1+ index))))
        :finally (return result)))

(defun format-code (code)
  (let* ((code          (remove-tex-escapes code))
         (package       (find-package '#:common-lisp))
         (read-client   (make-instance 'read-client :input           code
                                                    :current-package package))
         (stream        (make-string-output-stream))
         (render-client (make-instance 'highlight-client :stream stream)))
    (eclector.examples.highlight:highlight
     code                          ; :package (find-package #:common-)
     :read-client read-client
     :client      render-client)
    (get-output-stream-string stream)))
