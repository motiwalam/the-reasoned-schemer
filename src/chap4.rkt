#lang racket

(require (file "trs2-impl.rkt"))
(require (file "trs2-arith.rkt"))

(provide unwrapo)

;;; DEFINITIONS

(defrel (appendo l t out)
  (conde
   ((nullo l) (== t out))
   ((fresh (res a d)
           (conso a d l)
           (conso a res out)
           (appendo d t res)))))

(defrel (unwrapo x out)
  (conde
   ((fresh (a)
           
           (caro x a)
           (unwrapo a out)))
   ((== x out))))

;;; FRAMES

(define frame39
  (run 7 (x y)
       (appendo x y '(cake & ice d t))))

(define frame52
  (run 5 x
       (unwrapo x 'pizza)))

;;; EXERCISES