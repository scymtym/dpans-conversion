(cl:in-package #:dpans-conversion.transform)

(defun find-child-of-kind (builder kind node)
  (let ((children (bp:node-relation builder :element node)))
    (find-if (lambda (child)
               (eq (bp:node-kind builder child) kind))
             children)))

(defun evaluate-to-string (builder node)
  (labels ((rec (node)
             (case (bp:node-kind builder node)
               (:word
                (return-from evaluate-to-string
                  (getf (bp:node-initargs builder node) :content)))
               (:symbol
                (return-from evaluate-to-string
                  (getf (bp:node-initargs builder node) :name)))
               (t
                (map nil #'rec (bp:node-relation builder :element node))))))
    (rec node)))

(defun node-name (node)
  (let* ((builder 'list)
         (name    (bp:node-relation builder '(:name . 1) node)))
    (evaluate-to-string builder name)))

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
    ("Accessor"                  :function)
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

