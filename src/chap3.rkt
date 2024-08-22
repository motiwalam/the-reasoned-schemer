#lang racket

(require (file "trs2-impl.rkt"))
(require (file "trs2-arith.rkt"))

(require (file "chap2.rkt"))

(provide listo lolo membero)

;;; DEFINITIONS
(defrel (listo l)
  (conde
   ((nullo l))
   ((fresh (d)
           (cdro l d)
           (listo d)))))

(defrel (lolo l)
  (conde
   ((nullo l))
   ((fresh (a)
           (caro l a)
           (listo a))
    (fresh (d)
           (cdro l d)
           (lolo d)))))

(defrel (loso l)
  (conde
   ((nullo l))
   ((fresh (a)
           (caro l a)
           (singletono a))
    (fresh (d)
           (cdro l d)
           (loso d)))))

(defrel (membero x l)
  (conde
   ((caro l x))
   ((fresh (d)
           (cdro l d)
           (membero x d)))))

(defrel (proper-membero x l)
  (conde
   ((caro l x)
    (fresh (d)
           (cdro l d)
           (listo d)))
   ((fresh (d)
           (cdro l d)
           (proper-membero x d)))))
;;; FRAMES

(define frame23
  (run* q
        (fresh (x y)
               (lolo `((a b) (,x c) (d ,y))))))

(define frame74
  (run 12 l
       (proper-membero 'tofu l)))

;;; EXERCISES
; translate list?
(defrel (my-listo l)
  (conde
   ((nullo l) succeed)
   ((pairo l)
    (fresh (d)
           (cdro l d)
           (my-listo d)))))

; translate member?
(defrel (my-membero x l)
  (conde
   ((caro l x))
   ((fresh (d)
           (cdro l d)
           (my-membero x d)))))