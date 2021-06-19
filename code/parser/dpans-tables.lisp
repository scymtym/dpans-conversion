(cl:in-package #:dpans-conversion.parser)

(in-grammar dpans)

(defrule table-cell/inner (environment)
    (bounds (start end)
      (seq (* (<<- elements (and (not (or (column-separator) (row-terminator) (span)))
                                 (element environment))))
           (or (column-separator) (<<- spans (span)))))
  (let ((span (when spans (1+ (length spans)))))
    (bp:node* (:cell :span span :bounds (cons start end))
      (* :element (nreverse elements)))))

(defrule table-cell/last (environment)
    (bounds (start end)
      (seq (* (<<- elements (and (not (row-terminator)) (element environment))))
           (row-terminator)))
  (bp:node* (:cell :bounds (cons start end))
    (* :element (nreverse elements))))

(defrule table-row (environment)
    (bounds (start end)
      (seq (* (<<- cells (table-cell/inner environment)))
           (<<- cells (table-cell/last environment))
           (skippable*)))
  (bp:node* (:row :bounds (cons start end))
    (* :cell (nreverse cells))))

(defrule header (environment)
  (bounds (start end) (<<- elements (element environment)))
  (bp:node* (:header :bounds (cons start end))
    (* :element (nreverse elements))))

;;; Table macros

(defrule display-table (environment)
    (bounds (start end)
      (seq/ws (seq "\\display" (or "two" "three" "four" "five"))
               #\{ (<- caption (chunk)) #\}
               #\{ (* (<<- row (table-row environment))) #\}))
  (bp:node* (:table :which :display :bounds (cons start end))
    (1 (:caption . 1) caption)
    (* (:row     . *) (nreverse row))))

(defrule show-table (environment)
    (bounds (start end)
      (seq/ws (seq "\\show" (or "two" "three" "four" "five"))
              #\{ (<- caption (chunk)) #\}
              #\{ (* (<<- rows (table-row environment))) #\}))
  (bp:node* (:table :which :show :bounds (cons start end))
    (1 (:caption . 1) caption)
    (* (:row     . *) (nreverse rows))))

(defrule figure-table (environment)
    (bounds (start end)
      (seq/ws (seq "\\tablefig" (<- count (or (:transform "two"   2)
                                              (:transform "three" 3)
                                              (:transform "four"  4)
                                              (:transform "five"  5)
                                              (:transform "six"   6))))
              #\{ (<- caption (chunk)) #\}
              (* (seq/ws #\{ (<<- header (header environment)) #\})
                 count count)
              #\{ (* (<<- rows (table-row environment))) #\}))
  (bp:node* (:table :which :figure :bounds (cons start end))
    (1 (:caption . 1) caption)
    (* (:header  . *) (nreverse header))
    (* (:row     . *) (nreverse rows))))

(define-command tabletwo-entry
  (1 :term       (element environment))
  (1 :definition (element environment)))

(define-command tabletwo
  (2  :header (header environment))
  (1* :entry  (seq (skippable*) (tabletwo-entry environment))))

(defrule dpans-table (environment)
  (or (display-table environment)
      (show-table environment)
      (figure-table environment)
      (tabletwo environment)))
