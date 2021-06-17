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

(defrule display (environment) ; TODO use this
    (bounds (start end)
      (seq/ws (seq "\\display" (or "two" "three" "four" "five"))
               #\{ (<- caption (chunk)) #\}
               #\{ (* (<<- row (table-row environment))) #\}))
  (bp:node* (:displaytwo :bounds (cons start end))
    (1 (:caption . 1) caption)
    (* (:row     . *) (nreverse row))))

(define-command displaytwo
  (1  :caption (chunk))
  (1* :row     (table-row environment)))

(define-command displaythree
  (1  :caption (chunk))
  (1* :row     (table-row environment)))

(define-command displayfour
  (1  :caption (chunk))
  (1* :row     (table-row environment)))

(define-command displayfive
  (1  :caption (chunk))
  (1* :row     (table-row environment)))

(define-command showtwo
  (1  :caption (chunk))
  (1* :row     (table-row environment)))

(define-command showthree
  (1  :caption (chunk))
  (1* :row     (table-row environment)))

(define-command showfive
  (1  :caption (chunk))
  (1* :row     (table-row environment)))

(define-command tablefigtwo
  (1  :caption (chunk))
  (2  :header  (header environment))
  (1* :row     (table-row environment)))

(define-command tablefigthree
  (1  :caption (chunk))
  (3  :header  (header environment))
  (1* :row     (table-row environment)))

(define-command tablefigfour
  (1  :caption (chunk))
  (4  :header  (header environment))
  (1* :row     (table-row environment)))

(define-command tablefigsix
  (1  :caption (chunk))
  (6  :header  (header environment))
  (1* :row     (table-row environment)))

(define-command tabletwo-entry
  (1 :term       (element environment))
  (1 :definition (element environment)))

(define-command tabletwo
  (2  :header  (header environment))
  (1* :entry   (seq (skippable*) (tabletwo-entry environment))))

(defrule dpans-table (environment)
  (or (displaytwo environment)
      (displaythree environment)
      (displayfour environment)
      (displayfive environment)
      (showtwo environment)
      (showthree environment)
      (showfive environment)
      (tablefigtwo environment)
      (tablefigthree environment)
      (tablefigfour environment)
      (tablefigsix environment)
      (tabletwo environment)))
