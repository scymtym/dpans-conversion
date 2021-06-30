(cl:in-package #:dpans-conversion.transform)

(defun find-ancestor-of-kind (builder kind node)
  (bp:walk-nodes
   builder (lambda (recurse relation relation-args node kind* relations
                    &rest initargs &key &allow-other-keys)
             (declare (ignore relation relation-args relations initargs))
             (when (eq kind* kind)
               (return-from find-ancestor-of-kind node))
             (funcall recurse))
   node)
  nil)

(defun find-child-of-kind (builder kind node)
  (map nil (lambda (relation)
             (multiple-value-bind (relation* cardinality)
                 (bp:normalize-relation relation)
               (declare (ignore relation*))
               (ecase cardinality
                 ((1 bp:?)
                  (let ((child (bp:node-relation builder relation node)))
                    (when (eq (bp:node-kind builder child) kind)
                      (return-from find-child-of-kind child))))
                 (*
                  (map nil (lambda (child)
                             (when (eq (bp:node-kind builder child) kind)
                               (return-from find-child-of-kind child)))
                       (bp:node-relation builder relation node))))))
       (bp:node-relations builder node)))

(defun evaluate-to-string (builder node) ; TODO unused?
  (labels ((rec (node)
             (cond ((eq (bp:node-kind builder node) :symbol)             ; TODO name
                    (return-from evaluate-to-string
                      (let ((initargs (bp:node-initargs builder node)))
                        (values (getf initargs :name) (getf initargs :setf?)))))
                   ((a:when-let ((content (getf (bp:node-initargs builder node) :content)))
                      (return-from evaluate-to-string content)))
                   ((a:when-let ((content (getf (bp:node-initargs builder node) :name))) ; TODO unused?
                      (return-from evaluate-to-string content)))
                   (t
                    (map nil #'rec (bp:node-relation builder :element node))))))
    (rec node)))

(defun node-name (node)
  (let* ((builder 'list)
         (name    (bp:node-relation builder '(:name . 1) node)))
    (to-string builder name)))

(defun namespace<-ftype (ftype)
  (a:eswitch (ftype :test #'string=)
    ("Symbol"                    :symbol)
    ("Class"                     :type)
    ("System Class"              :type)
    ("Condition Type"            :type)
    ("Type"                      :type)
    ("Type Specifier"            :type)
    ("Macro"                     :macro)
    ("Local Macro"               :macro)
    ("Function"                  :function)
    ("Local Function"            :function)
    ("Accessor"                  (values :function t))
    ("Standard Generic Function" :function)
    ("Special Operator"          :special-operator)
    ("Variable"                  :variable)
    ("Constant Variable"         :constant)
    ("Declaration"               :declaration)
    ("Restart"                   :restart)))

(defun stemmify (string)
  (cond ((string= "written" string)
         "write")
        ((or (a:ends-with-subseq "ses" string) ; classes
             (a:ends-with-subseq "xes" string)) ; complexes
         (subseq string 0 (- (length string) 2)))
        ((a:ends-with-subseq "ding" string) ; upgrading
         (concatenate 'string
                      (subseq string 0 (- (length string) 3))
                      "e"))
        ((or (a:ends-with-subseq "tes" string)  ; attributes
             (a:ends-with-subseq "pes" string)  ; types
             (a:ends-with-subseq "mes" string)  ; names
             (a:ends-with-subseq "ges" string)) ; packages
         (subseq string 0 (- (length string) 1)))
        ((a:ends-with-subseq "es" string)
         (subseq string 0 (- (length string) 2)))
        ((a:ends-with-subseq "ing" string)
         (subseq string 0 (- (length string) 3)))
        ((or (a:ends-with-subseq "oaded" string) ; loaded
             (a:ends-with-subseq "ned"   string)
             (a:ends-with-subseq "wed"   string)
             (a:ends-with-subseq "nded"  string) ; expanded
             (a:ends-with-subseq "shed"  string)
             (a:ends-with-subseq "sed"   string) ; accessed
             (a:ends-with-subseq "ted"   string) ; reported
             (a:ends-with-subseq "lled"  string))
         (subseq string 0 (- (length string) 2)))
        ((a:ends-with-subseq "ed" string)
         (subseq string 0 (1- (length string))))
        ((a:ends-with #\s string)
         (subseq string 0 (1- (length string))))
        (t
         string)))
