#lang racket

(require (file "trs2-impl.rkt"))
(require (file "trs2-arith.rkt"))

;;; DEFINITIONS

(defrel (memo x l out)
  (conde
   ((caro l x) (== l out))
   ((fresh (d)
           (cdro l d)
           (memo x d out)))))

(defrel (rembero x l out)
  (conde
   ((nullo l) (nullo out))
   ((conso x out l))
   ((fresh (a d res)
           (conso a d l)
           (== (cons a res) out)
           (rembero x d res)))))

;;; FRAMES

(define frame5
  (run* q
        (memo 'fig '(pea) '(pea))))

(define frame14
  (run* out
        (memo 'fig '(fig fig pea) out)))
;;; EXERCISES
; translate rember
(defrel (my-rembero x l out)
  (conde
   ((nullo l) (nullo out))
   ((fresh (d)
           (conso x d l)
           (== out d)))
   ((fresh (a d res)
           (conso a d l)
           (== (cons a res) out)
           (my-rembero x d res)))))