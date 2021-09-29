(cl:in-package #:dpans-conversion.clim)

(defclass display (transform:builder-mixin)
  ((%stream :initarg  :stream
            :accessor stream)
   ;; Parameters
   (%annotations :initarg  :annotations
                 :type     list
                 :reader   annotations
                 :initform '())
   (%highlight   :initarg  :highlight
                 :type     (or null function)
                 :reader   highlight
                 :initform nil)
   ;; State
   (%context     :accessor context
                 :initform nil)
   (%depth       :initarg  :depth
                 :accessor depth
                 :initform 1))
  (:default-initargs
   :stream (a:required-argument :stream)))

(defmethod transform:transform-node
    ((transform display) recurse relation relation-args
     node (kind t) relations &key)
  (break "~A" kind)
  (when (plusp (depth transform))
    (funcall recurse)))

(defmethod transform:transform-node :around
    ((transform display) recurse relation relation-args
     node (kind t) relations &rest initargs &key)
  (a:if-let ((highlight (highlight transform)))
    (flet ((do-highlight ()
             (let ((stream (stream transform))
                   x y)
               (clim:surrounding-output-with-border
                   (stream :shape      :rounded
                           :background clim:+dark-orange+
                           :padding    1
                           :radius     2)
                 (call-next-method)
                 (setf (values x y) (clim:stream-cursor-position stream)))
               (setf (clim:stream-cursor-position stream) (values (+ x 3) y)))))
      (cond ((eq highlight node)
             (do-highlight))
            ((and (functionp highlight )
                  (apply highlight relation relation-args node kind relations initargs))
             (do-highlight))
            (t
             (call-next-method))))
    (call-next-method)))

(defmacro define-display ((kind &rest key) &body body)
  `(defmethod transform:transform-node
       ((transform display) recurse relation relation-args
        node (kind (eql ,kind)) relations
        &rest initargs &key ,@key)
     (declare (ignorable transform recurse relation relation-args
                         node kind relations initargs))
     (let ((builder (transform:builder transform))
           (stream  (stream transform)))
       (declare (ignorable builder stream))
       (flet ((recurse (&rest relations)
                  (if (null relations)
                      (funcall recurse)
                      (funcall recurse :relations relations))))
         (declare (ignorable #'recurse )
                  (inline recurse))
         ,@body))))

(defun clean-whitespace (string)
  (with-output-to-string (stream)
    (let ((in-whitespace? nil))
      (map nil (lambda (character)
                 (case character
                   ((#\Space #\Tab #\Newline)
                    (unless in-whitespace?
                      (write-char #\Space stream)
                      (setf in-whitespace? t)))
                   (t
                    (write-char character stream)
                    (setf in-whitespace? nil))))
           string))))

(define-display (:chunk content)
  (let* ((context (context transform))
         (string  (case context
                    (:listing content)
                    (t        (clean-whitespace content))))) ; HACK the parser should do this
    (case context
      (:math (clim:with-drawing-options (stream :text-face :italic)
               (write-string string stream)))
      (t     (write-string string stream)))))

(define-display (:title)
  (clim:with-drawing-options (stream :text-size :huge)
    (recurse '(:name . 1)))
  (terpri stream))

(define-display (:sub-title)
  (clim:with-drawing-options (stream :text-size :large)
    (recurse '(:name . *)))
  (terpri stream))

(define-display (:chapter)
  (flet ((draw-it (stream)
           (clim:with-drawing-options (stream :text-face :bold
                                              :text-size 20)
             (climi::letf (((stream transform) stream))
               (recurse '(:name . 1))))))
    (cond ((eql (depth transform) 1)
           (clim:with-output-as-presentation (stream node 'link)
             (draw-it stream))
           (terpri stream))
          (t
           (draw-it stream)
           (terpri stream)
           (decf (depth transform))
           (unwind-protect
                (clim:indenting-output (stream '(2 :character))
                  (climi::letf (((stream transform) stream))
                    (recurse '(:element . *))))
             (incf (depth transform)))))))

;; TODO make with-section, use for this, chapter and proposal
(define-display (:section name level) ; TODO either initarg or relation
  (flet ((draw-it (stream)
           (clim:with-drawing-options (stream :text-face :bold
                                              :text-size (- 20 (* 2 (or level 1))))
             (a:if-let ((relation (find :name relations :key #'car))) ; HACK
               (climi::letf (((stream transform) stream))
                 (recurse relation))
               (write-string name stream)))))
    (cond ((eql (depth transform) 1)
           (clim:with-output-as-presentation (stream node 'link)
             (draw-it stream))
           (terpri stream))
          (t
           (draw-it stream)
           (terpri stream)
           (decf (depth transform))
           (unwind-protect
                (recurse '(:element . *))
             (incf (depth transform)))))))

(define-display (:cell)
  (clim:formatting-cell (stream)
    (recurse '(:element . *))))

(define-display (:row)
  (clim:formatting-row (stream)
    (recurse '(:cell . *))))

(define-display (:table)
  (clim:formatting-table (stream)
    (recurse '(:row . *))))

(define-display (:figure)
  (fresh-line stream)
  (climi::with-preserved-cursor-x (stream)
    (clim:surrounding-output-with-border (stream :shape  :rounded
                                                 :radius 3)
      (clim:formatting-table (stream)
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :center)
            (recurse '(:element . *))))
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :center)
            (clim:with-drawing-options (stream :text-face :italic)
              (recurse '(:caption . 1)))))))))

;;; Typographic

(define-display (:dash which)
  (let ((string (ecase which
                  (:en "–")
                  (:em "—"))))
    (write-string string stream)))

(define-display (:f)
  (break "should not happen")
  (clim:with-drawing-options (stream :text-family :fix :ink clim:+purple1+)
    (recurse)))

(macrolet ((define (kind &rest options)
             `(define-display (,kind)
                (clim:with-drawing-options (stream ,@options)
                  (recurse)))))
  (define :typewriter :text-family :fix) ; TODO rename to monospace
  (define :roman      :text-face   :roman)
  (define :bold       :text-face   :bold)
  (define :italic     :text-face   :italic))

(define-display (:subscript)
  (recurse '(:left . *))
  (climi::with-preserved-cursor-y (stream)
    (let ((dy (* 1/3 (nth-value 1 (clim:text-size stream "M")))))
      (clim:stream-increment-cursor-position stream 0 dy))
    (clim:with-drawing-options (stream :text-size :smaller)
      (recurse '(:right . *)))))

(define-display (:superscript)
  (recurse '(:left . *))
  (climi::with-preserved-cursor-y (stream)
    (let ((dy (* 1/3 (nth-value 1 (clim:text-size stream "M")))))
      (clim:stream-increment-cursor-position stream 0 (- dy)))
    (clim:with-drawing-options (stream :text-size :smaller)
      (recurse '(:right . *)))))

(define-display (:paragraph-break)
  (terpri stream)
  (terpri stream))

(define-display (:enumeration-item)
  (let ((context (context transform)))
    (destructuring-bind (kind index indent) context
      (declare (ignore kind))
      (clim:formatting-cell (stream)
        (let ((bullet (format nil "~D. " (1+ index))))
          (write-string bullet stream)
          (clim:indenting-output (stream indent)
            (climi::letf (((stream transform) stream))
              (recurse))))
        (incf (second context))))))

(define-display (:enumeration-list)
  (fresh-line stream)
  (let* ((items  (bp:node-relation builder '((:element . *)) node))
         (indent (format nil "~D. " (length items))))
    (climi::letf (((context transform) (list kind 0 indent)))
      (clim:formatting-item-list (stream :n-columns 1)
        (recurse)))))

(define-display (:list-item)
  (clim:formatting-cell (stream)
    (let ((bullet  "• "))
      (write-string bullet stream)
      (clim:indenting-output (stream bullet)
        (climi::letf (((stream transform) stream))
          (recurse))))))

(define-display (:item-list)
  (fresh-line stream)
  (clim:formatting-item-list (stream :n-columns 1)
    (recurse)))

(define-display (:definition-item)
  (clim:formatting-cell (stream)
    (clim:with-drawing-options (stream :text-face :bold)
      (recurse '(:key . *)))
    (terpri stream)
    (clim:indenting-output (stream '(2 :character))
      (climi::letf (((stream transform) stream))
        (recurse '(:body . *))))))

(define-display (:definition-list)
  (fresh-line stream)
  (clim:formatting-item-list (stream :n-columns 1)
    (recurse)))

(defun color<-hex (hex)
  (let ((red   (ldb (byte 8 16) hex))
        (green (ldb (byte 8  8) hex))
        (blue  (ldb (byte 8  0) hex)))
    (clim:make-rgb-color (/ red #xff) (/ green #xff) (/ blue #xff))))

(define-display (:syntax classes)
  (labels ((%has-class? (class)
             (if (listp classes)
                 (member class classes :test #'string=)
                 (string= class classes)))
           (has-class? (class &rest more-classes)
             (or (%has-class? class)
                 (some #'%has-class? more-classes))))
    (cond ((has-class? "comment")
           (clim:with-drawing-options (stream :ink clim:+gray33+)
             (recurse)))
          ((has-class? "keyword-symbol")
           (clim:with-drawing-options (stream :ink (color<-hex #x902010))
             (recurse)))
          ((has-class? "lambda-list-keyword-symbol")
           (clim:with-drawing-options (stream :ink (color<-hex #x909010))
             (recurse)))
          ((has-class? "interned-symbol" "standard-symbol")
           (clim:with-drawing-options (stream :ink clim:+brown4+)
             (recurse)))
          ((has-class? "number")
           (clim:with-drawing-options (stream :ink (color<-hex #x404080))
             (recurse)))
          ((has-class? "string" "character")
           (clim:with-drawing-options (stream :ink (color<-hex #x408040))
             (recurse)))
          ((has-class? "quote")
           (let (x y)
             (clim:surrounding-output-with-border (stream :shape      :rectangle
                                                          :padding    0
                                                          :background clim:+gray94+
                                                          :ink        clim:+transparent-ink+
                                                          :move-cursor nil)
               (recurse)
               (setf (values x y) (clim:stream-cursor-position stream)))
             (setf (clim:stream-cursor-position stream) (values x y))))
          (t
           (recurse)))))

(define-display (:listing inline?)
  (if inline?
      (clim:with-drawing-options (stream :text-family :fix)
        (recurse))
      (clim:with-bounding-rectangle* (x1) (clime:stream-page-region stream)
        (let (x y)
          (clim:surrounding-output-with-border (stream :shape      :rounded
                                                       :radius     3
                                                       :background clim:+gray90+
                                                       :ink        clim:+gray30+)
            (clime:with-temporary-margins (stream :left `(:absolute ,x1))
              (climi::letf (((context transform) :listing)
                            ((clim:stream-end-of-line-action stream) :allow))
                (clim:with-drawing-options (stream :text-family :fix)
                  (recurse)
                  (setf (values x y) (clim:stream-cursor-position stream))))))
          (setf (clim:stream-cursor-position stream) (values x y))))))

;;; Math

(define-display (:over)
  (climi::with-preserved-cursor-y (stream)
    (let* ((stream (stream transform))
           (style  (clim:make-text-style nil nil :smaller))
           (height (nth-value 1 (clim:text-size stream "M" :text-style style))))
      (clim:stream-increment-cursor-position stream 0 (* -.3 height))
      (let ((record (clim:with-new-output-record (stream)
                      (clim:with-drawing-options (stream :text-style style)
                        (clim:formatting-table (stream :x-spacing 0 :y-spacing 0)
                          (clim:formatting-row (stream)
                            (clim:formatting-cell (stream :align-x :center :align-y :bottom)
                              (recurse '(:left . *))))
                          (clim:formatting-row (stream)
                            (clim:formatting-cell (stream :align-x :center :align-y :top)
                              (recurse '(:right . *)))))))))
        (clim:with-bounding-rectangle* (:x1 x1 :x2 x2 :center-y y) record
          (clim:draw-line* stream x1 (1- y) x2 (1- y)))))))

(defmethod transform:transform-node :around ; TODO make a mixin for this?
    ((transform display) recurse relation relation-args node
     (kind (eql :other-command-application)) relations
     &key name)
  (let ((stream (stream transform)))
    (cond ((not (eq (context transform) :math))
           (call-next-method))
          ((a:when-let ((primitive (tex:find-primitive name)))
             (when (member :math (tex:modes primitive))
               (cond ((typep primitive 'tex::math-symbol)
                      (princ (tex::character primitive) stream))
                     ((string= name "sqrt")
                      (write-string "√" stream)
                      (funcall recurse))
                     ((string= name "underline")
                      (climi::with-preserved-cursor-y (stream)
                        (clim:surrounding-output-with-border (stream :shape :underline)
                          (climi::letf (((stream transform) stream))
                            (funcall recurse)))))
                     ((string= name "buildrel")
                      (clim:with-drawing-options (stream :text-size :smaller)
                        (funcall recurse)))
                     ((member :meta-delimiter (tex:tags primitive))
                      (funcall recurse))
                     (t
                      (format stream "\\~A" name)
                      (funcall recurse :function  (lambda (recurse relation-args relation node kind relations
                                                           &rest initargs)
                                                    (write-string "{" stream)
                                                    (apply #'transform:transform-node
                                                           transform recurse relation-args relation node kind relations
                                                           initargs)
                                                    (write-string "}" stream))
                                       :relations '((:argument . *)))))
               t)))
          (t
           (clim:with-drawing-options (stream :ink clim:+red+)
             (let ((*print-level* 2) (*print-circle* t))
               (format stream "unexpanded non-math macro: ~S" node)))
           #+TODO (call-next-method)))))

(define-display (:math-display)
  (fresh-line stream)
  (climi::letf (((context transform) :math))
    (recurse))
  (terpri stream))

(define-display (:math)
  (climi::letf (((context transform) :math))
    (recurse)))

;;; Semantic markup

(define-display (:param)
  (clim:with-drawing-options (stream :text-face :italic)
    (recurse)))

(define-display (:gentry)
  (clim:with-drawing-options (stream :text-face :bold)
    (recurse '(:name . 1)))
  (write-string " " stream)
  (recurse '(:body . *)))

(define-display (:term)
  (recurse))

(define-display (:newterm)
  (recurse))

(define-display (:name content)
  (clim:with-drawing-options (stream :text-family :fix)
    (write-string content stream)))

(define-display (:symbol name setf?)
  (clim:with-drawing-options (stream :text-family :fix)
    (when setf?
      (write-string "(setf " stream))
    (write-string name stream)
    (when setf?
      (write-string ")" stream))))

(define-display (:keyword)
  (clim:with-drawing-options (stream :text-family :fix :ink clim:+firebrick+)
    (recurse)))

(define-display (:lambda-list-keyword which) ; TODO should this be a link?
  (clim:with-drawing-options (stream :text-family :fix :ink clim:+firebrick+)
    (format stream "&~(~A~)" which)))

(defun display-name (name setf? stream)
  (when setf?
    (write-string "(setf " stream))
  (write-string name stream)
  (when setf?
    (write-string ")" stream)))

(define-display (:eql-specializer)
  (write-string "(eql " stream)
  (recurse)
  (write-string ")" stream))

(define-display (:specialized-parameter)
  (clim:with-drawing-options (stream :text-face :roman)
    (write-string "(" stream))
  (recurse '(:name . 1))
  (clim:with-drawing-options (stream :text-face :roman)
    (write-string " " stream)
    (recurse '(:specializer . 1))
    (write-string ")" stream)))

(define-display (:call-syntax which)
  (fresh-line stream)
  (clim:with-drawing-options (stream :text-family :fix)
    (clim:with-drawing-options (stream :text-face :bold)
      (if (find '(:name . 1) relations :test #'equal)
          (recurse '(:name . 1))
          (funcall recurse :relations '((:name . *))
                           :function  (let ((first? t)) ; TODO same in :component display
                                        (lambda (&rest args)
                                          (if first?
                                              (setf first? nil)
                                              (write-string ", " stream))
                                          (when (eq which :setf)
                                            (write-string "(setf (" stream))
                                          (apply #'transform:transform-node
                                                 transform args))))))
    (write-char #\Space stream)
    ;; Function-like
    (when (find '(:argument . *) relations :test #'equal) ; TODO should use which instead
      (clim:with-drawing-options (stream :text-face :italic)
        (recurse '(:argument . *)))
      (when (eq which :setf)
        (write-string ") " stream)
        (clim:with-drawing-options (stream :text-face :italic)
          (recurse '(:new-value . 1)))
        (write-string ") " stream))
      (cond ((find '(:return-value . *) relations :test #'equal)
             (write-string " → " stream)
             (clim:with-drawing-options (stream :text-face :italic)
               (recurse '(:return-value . *))))
            ((member which '(:method :setf)))
            (t
             (write-string " → |" stream))))
    ;; Type specifier
    (when (find '(:element . *) relations :test #'equal)
      (clim:with-drawing-options (stream :text-face :italic)
        (recurse '(:element . *))))))

;;; Component

(define-display (:ftype)
  (clim:with-drawing-options (stream :ink       clim:+forest-green+
                                     :text-size :larger)
    (recurse)))

(define-display (:none)
  (clim:with-drawing-options (stream :text-face :italic :ink clim:+gray30+)
    (write-string "None" stream)))

(define-display (:part)
  (fresh-line stream)
  (clim:with-drawing-options (stream :text-face :bold)
    (recurse '(:name . 1)))
  (terpri stream)
  (clim:indenting-output (stream '(2 :character))
    (climi::letf (((stream transform) stream))
      (recurse '(:element . *)))))

(define-display (:component)
  (decf (depth transform)) ; TODO make a macro or an :around method with (if (section-like? kind) (decf (depth ...
  (unwind-protect
       (clim:with-bounding-rectangle* (:x2 x2 :width width)
           (clime:stream-page-region stream)
         (progn #+no clim:updating-output #+no (stream :cache-value (cons node width)
                                                       :cache-test  (lambda (value1 value2)
                                                                      (and (eq  (car value1) (car value2))
                                                                           (eql (cdr value1) (cdr value2)))))
                (clim:surrounding-output-with-border (stream :shape      :rounded
                                                             :radius     3
                                                             :background clim:+beige+
                                                             :ink        clim:+gray30+)
                  (clim:formatting-table (stream :y-spacing '(.5 :character))
                    ;; Name(s) and "ftype"
                    (clim:formatting-row (stream)
                      (clim:formatting-cell (stream)
                        (clim:with-drawing-options (stream :text-face :bold
                                                           :text-size :larger)
                          (let ((first? t))
                            (funcall recurse :function  (lambda (&rest args) ; TODO same in call syntax, make a helper function
                                                          (if first?
                                                              (setf first? nil)
                                                              (write-string ", " stream))
                                                          (apply #'transform:transform-node
                                                                 transform args))
                                             :relations '((:name . *)))))
                        (let ((record (clim:with-new-output-record (stream)
                                        (recurse '(:ftype . 1)))))
                          (clim:with-bounding-rectangle* (:y1 y1 :width width) record
                            (let ((x1 (- x2 width)))
                              (setf (clim:output-record-position record) (values x1 y1)))))))
                    ;; Body
                    (clim:formatting-row (stream)
                      (clim:formatting-cell (stream)
                        (clime:with-temporary-margins (stream :left  `(:absolute 0)
                                                              :right `(:absolute ,width))
                          (climi::letf (((stream transform) stream))
                            (recurse '(:element . *))))))))))
    (incf (depth transform)))
  (terpri stream)
  (terpri stream))

;;; References

(defun call-with-output-as-link (continuation stream target)
  (a:if-let ((target (if (typep target 'transform::reference) ; TODO should be handled in transform module
                         (transform::target target)
                         target)))
    (clim:with-output-as-presentation (stream target 'link)
      (funcall continuation stream))
    (clim:with-drawing-options (stream :ink clim:+red+)
      (funcall continuation stream))))

(defmacro with-output-as-link ((stream target) &body body)
  (check-type stream symbol)
  `(call-with-output-as-link (lambda (,stream) ,@body) ,stream ,target))

(defun ink<-namespace (namespace)
  (case namespace ; TODO will be ecase
    (:section             clim:+black+)
    (:figure              clim:+black+)
    (:reviewer-note       clim:+black+)
    (:editor-note         clim:+black+)
    (:issue               clim:+black+)
    (:proposal            clim:+black+)

    (:function            clim:+dark-blue+)
    (:type                clim:+forest-green+)
    (:declaration         clim:+dark-orange+)
    (:variable            clim:+dark-red+)
    (:constant            clim:+brown+)
    (:macro               clim:+dark-violet+)
    (:package             clim:+dark-olive-green+)
    (:lambda-list-keyword clim:+brown+)
    (:special-operator    clim:+violet+)

    (:restart             clim:+dark-orange+)
    (:symbol              clim:+dark-orange+)
    (:keyword             clim:+dark-orange+)
    (:index/code          clim:+dark-orange+)

    (:glossary            (color<-hex #x400000))))

(define-display (:index))

(define-display (:reference target name namespace)
  (with-output-as-link (stream target)
    (clim:with-drawing-options (stream :ink (if target
                                                (ink<-namespace namespace)
                                                clim:+red+))
      (if name
          (write-string name stream)
          (write-string "<missing name>" stream)))))

(define-display (:secref target)
  (recurse))

(define-display (:possible-reference target)
  (assert (null target))
  (clim:with-drawing-options (stream :ink clim:+red+)
    (recurse)))

(define-display (:macref target) ; TODO i don't think these references should occur
  (assert (null target))
  (clim:with-drawing-options (stream :ink clim:+red+)
    (recurse)))

(define-display (:funref target)
  (clim:with-output-as-presentation (stream target 'link)
    (clim:with-drawing-options (stream :ink clim:+dark-blue+)
      (recurse))))

(define-display (:typeref target)
  (clim:with-output-as-presentation (stream target 'link)
    (clim:with-drawing-options (stream :ink clim:+forest-green+)
      (recurse))))

(define-display (:varref target)
  (clim:with-output-as-presentation (stream target 'link)
    (clim:with-drawing-options (stream :ink clim:+dark-red+)
      (recurse))))

(define-display (:miscref target)
  (recurse))

(define-display (:newtermidx target)
  (recurse))

;;; Annotations

(define-display (:issue-annotation)
  (flet ((do-it ()
           (recurse)))
    (if (find kind (annotations transform))
        (let ((ink (clim:make-contrasting-inks 8 (random 8))))
          (clim:surrounding-output-with-border (stream :ink ink)
            (do-it)))
        (do-it))))

(flet ((display-note (transform node kind person content stream)
         (cond ((eql (depth transform) 2)
                (when person
                  (clim:with-drawing-options (stream :text-face :italic)
                    (write-string person stream))
                  (write-string ": " stream))
                (write-string content stream))
               ((find kind (annotations transform))
                (with-output-as-link (stream node)
                  (write-string "▶" stream))))))

  (define-display (:editor-note editor content)
    (display-note transform node kind editor content stream))

  (define-display (:reviewer-note reviewer content)
    (display-note transform node kind reviewer content stream)))

;;; Issues

(define-display (:issue-reference)
  )

(define-display (:forum)
  )

(define-display (:category)
  )

(define-display (:line) ; TODO should not happen
  (recurse))

(defun color<-status (status)
  (case status
    (:passed (color<-hex #xa0e0a0))
    (t       clim:+beige+)))

(define-display (:proposal name status)
  (climi::with-preserved-cursor-x (stream)
    (clim:surrounding-output-with-border (stream :shape      :rounded
                                                 :radius     2
                                                 :background (color<-status status))
      (clim:with-drawing-options (stream :ink        (ink<-namespace :proposal)
                                         :text-face :bold
                                         :text-size :larger)
        (format stream "Proposal ~A" name))
      (terpri stream)
      (clim:indenting-output (stream '(2 :character))
        (climi::letf (((stream transform) stream))
          (recurse))))))

(define-display (:issue)
  (cond ((zerop (depth transform)))
        ((not (eq node (node (state (clim:find-pane-named clim:*application-frame* 'content))))) ; TODO
         (link node stream (lambda (stream)
                             (declare (ignore stream))
                             (recurse '(:name . 1) ))))
        (t
         (recurse))))

;;;

(define-display (:collection)
  (recurse))

(define-display (:output-file)
  (break "should not happen")
  (recurse))

(define-display (:input) ; TODO should not happen
  (recurse))

(define-display (:file)
  (recurse))

(define-display (:splice)
  (recurse))

(define-display (:block) ; TODO should not happen
  (if (some (lambda (element)
               (eq (bp:node-kind builder element) :bnf-rule))
             (bp:node-relation builder '(:element . *) node)) ; TODO hack
      (clim:formatting-table (stream)
        (recurse))
      (recurse)))

(define-display (:argument) ; TODO should not happen
  (clim:with-drawing-options (stream :ink clim:+red+)
    (format stream "unresolved argument")))

;;; TODO

(define-display (:other-command-application name) ; TODO should not happen
  (clim:with-drawing-options (stream :ink clim:+red+)
    (format stream "unexpanded macro: ~A" name)))

(define-display (:hbox) ; TODO should not happen
  (clim:with-drawing-options (stream :ink clim:+red+)
    (format stream "hbox"))
  (recurse))

(define-display (:bnf-rule)
  (clim:formatting-row (stream)
    (clim:formatting-cell (stream :align-y :center)
      (recurse '(:name . 1)))
    (clim:formatting-cell (stream :align-y :center)
      (write-string "::=" stream))
    (clim:formatting-cell (stream)
      (recurse '(:element . *))))) ; TODO

(define-display (:vtop)
  (recurse))

(define-display (:vbox))

(define-display (:column-separator) ; TODO
  (break "should not happen")
  (clim:with-drawing-options (stream :ink clim:+red+)
    (write-string "\\&" stream)))
