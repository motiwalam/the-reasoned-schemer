#lang racket

(require (file "trs2-impl.rkt"))
(require (file "trs2-arith.rkt"))

(provide pairo singletono)

;;; DEFINITIONS
;; N.B. some of these are already defined in trs2-arith
(defrel (caro p a)
  (fresh (d)
         (== (cons a d) p)))
(defrel (cdro p d)
  (fresh (a)
         (== (cons a d) p)))
(defrel (conso a d p)
  (== (cons a d) p))  ; slightly different from the book
(defrel (pairo p)
  (fresh (a d)
         (conso a d p)))
(defrel (nullo p)
  (== '() p))
(defrel (singletono l)
  (fresh (d)
         (cdro l d)
         (nullo d)))

;;; FRAMES 
(define frame5 (run* r
                     (fresh (x y)
                            (caro `(,r ,y) x)
                            (== 'pear x))))

(define frame8 (run* r
                     (fresh (x y)
                            (caro '(grape raisin pear) x)
                            (caro '((a) (b) (c)) y)
                            (== (cons x y) r))))

(define frame12 (run* r
                      (fresh (v)
                             (cdro '(a c o r n) v)
                             (fresh (w)
                                    (cdro v w)
                                    (caro w r)))))
(define frame12.2 (run* r
                        (fresh (v w)
                               (cdro '(a c o r n) v)
                               (cdro v w)
                               (caro w r))))

(define frame17 (run* x
                      (cdro '(c o r n) `(,x r n))))

(define frame18 (run* l
                      (fresh (x)
                             (cdro l '(c o r n))
                             (caro l x)
                             (== 'a x))))

(define frame27 (run* l
                      (fresh (d t x y w)
                             (conso w '(n u s) t) ; t = `(,w n u s)
                             (cdro l t)           ; l = `(,a ,w n u s)
                             (caro l x)           ; l = `(,x ,w n u s)
                             (== 'b x)            ; l = `(b ,w n u s)
                             (cdro l d)           ; d = t
                             (caro d y)           ; y = w
                             (== 'o y)            ; y = w = 'o
                             ;; l = `(b o n u s)
                             )))

(define frame45 (run* r
                      (fresh (x y)
                             (== (cons x (cons y 'salad)) r))))

;;; EXERCISES
; define caro and cdro with conso
(defrel (caro2 p a)
  (fresh (d) (conso a d p)))
(defrel (cdro2 p d)
  (fresh (a) (conso a d p)))
