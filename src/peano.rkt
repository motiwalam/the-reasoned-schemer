#lang racket

(require (file "trs2-impl.rkt"))
(require (file "trs2-arith.rkt"))

;; zero is empty list
(define zero '())
(define zeroo nullo)

;; succ is wrapping in a singleton list
(define (succ x) `(,x))
(define (succo Sn n)
  (== Sn `(,n)))

;; simple conversion functions back and forth
(define (peano2num p)
  (match p
    ['()   0]
    [`(,k) (+ 1 (peano2num k))]))

(define (num2peano n)
  (if (equal? n 0)
      zero
      (succ (num2peano (- n 1)))))

;; apply to the result of run* when all variables
;; should be numbers
(define tonums (curry map (curry map peano2num)))

(defrel (addo m n m+n)
  (conde
   [(zeroo m) (== m+n n)]
   [(fresh (k k+n)
           (succo m k)
           (== m+n (succ k+n))
           (addo k n k+n))]))

(defrel (mulo m n m*n)
  (conde
   [(zeroo m) (zeroo m*n)]
   [(fresh (k k*n)
           (succo m k)
           (mulo k n k*n)
           (addo k*n n m*n))]))
           