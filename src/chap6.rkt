#lang racket

(require (file "trs2-impl.rkt"))
(require (file "trs2-arith.rkt"))

;;; DEFINITIONS
(defrel (alwayso)
  (conde
   (succeed)
   ((alwayso))))

;;; FRAMES

;;; EXERCISES

; conde draws from each stream one at a time
(defrel (garlic-onion q)
  (conde
   ((== 'garlic q) (alwayso))
   ((== 'onion q) (alwayso))))
(define go*3 (run 6 q (garlic-onion q)))