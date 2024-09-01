#lang racket

(require (file "trs2-impl.rkt"))
(require (file "trs2-arith.rkt"))

;;; DEFINITIONS

;;; FRAMES

;;; EXERCISES

(defrel (sumo int1 int2 out)
  
  (fresh (s1 m1 s2 m2 so mo)
         (conso s1 m1 int1)
         (conso s2 m2 int2)
         (conso so mo out)

         (conde
          ;; if int1, int2 >= 0
          [(== s1 0) (== s2 0) (== so 0) (pluso m1 m2 mo)]
          [(== s1 0) (== s2 1) 'TODO]
          [(== s1 1) (== s2 0) 'TOOD]
          [(== s1 1) (== s2 1) (== so 1) (pluso m1 m2 mo)])))