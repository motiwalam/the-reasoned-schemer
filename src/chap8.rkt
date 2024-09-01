#lang racket

(require (file "trs2-impl.rkt"))
(require (file "trs2-arith.rkt"))

;;; DEFINITIONS

(defrel (*o n m p)
  (conde
    ;; n = 0 -> p = 0
    ((== '() n) (== '() p))
    ;; n > 0, m = 0 -> p = 0
    ((poso n) (== '() m) (== '() p))
    ;; n = 1, m > 0 -> p = m
    ((== '(1) n) (poso m) (== m p))
    ;; n > 1, m = 1 -> p = n
    ((>1o n) (== '(1) m) (== n p))
    ;; n = 2*x for x > 0
    ;; -> p = 2 * z where z > 0 and
    ;; x * m = z
    ((fresh (x z)
       (== `(0 . ,x) n) (poso x)
       (== `(0 . ,z) p) (poso z)
       (>1o m)
       (*o x m z)))
    ;; n = 2x + 1 where x > 0
    ;; and m = 2y where y > 0
    ;; n * m = m * n
    ((fresh (x y)
       (== `(1 . ,x) n) (poso x)
       (== `(0 . ,y) m) (poso y)
       (*o m n p)))
    
    ((fresh (x y)
       (== `(1 . ,x) n) (poso x)      
       (== `(1 . ,y) m) (poso y)
       (odd-*o x n m p)))))

(defrel (odd-*o x n m p)
  (fresh (q)
    (bound-*o q p n m)
    (*o x m q)
    (pluso `(0 . ,q) m p)))

(defrel (bound-*o q p n m)
  (conde
    ((== '() q) (poso p))
    ((fresh (a0 a1 a2 a3 x y z)
       (== `(,a0 . ,x) q)
       (== `(,a1 . ,y) p)
       (conde
         ((== '() n)
          (== `(,a2 . ,z) m)
          (bound-*o x y z '()))
         ((== `(,a3 . ,z) n) 
          (bound-*o x y z m)))))))

(defrel (=lo n m)
  (conde
    ((== '() n) (== '() m))
    ((== '(1) n) (== '(1) m))
    ((fresh (a x b y)
       (== `(,a . ,x) n) (poso x)
       (== `(,b . ,y) m) (poso y)
       (=lo x y)))))

(defrel (<lo n m)
  (conde
    ((== '() n) (poso m))
    ((== '(1) n) (>1o m))
    ((fresh (a x b y)
       (== `(,a . ,x) n) (poso x)
       (== `(,b . ,y) m) (poso y)
       (<lo x y)))))

(defrel (<=lo n m)
  (conde
    ((=lo n m))
    ((<lo n m))))

(defrel (<o n m)
  (conde
    ((<lo n m))
    ((=lo n m)
     (fresh (x)
       (poso x)
       (pluso n x m)))))

(defrel (<=o n m)
  (conde
    ((== n m))
    ((<o n m))))

(defrel (splito n r l h)
  (conde
    ((== '() n) (== '() h) (== '() l))
    ((fresh (b n^)
       (== `(0 ,b . ,n^) n) (== '() r)
       (== `(,b . ,n^) h) (== '() l)))
    ((fresh (n^)
       (==  `(1 . ,n^) n) (== '() r)
       (== n^ h) (== '(1) l)))
    ((fresh (b n^ a r^)
       (== `(0 ,b . ,n^) n)
       (== `(,a . ,r^) r) (== '() l)
       (splito `(,b . ,n^) r^ '() h)))
    ((fresh (n^ a r^)
       (== `(1 . ,n^) n)
       (== `(,a . ,r^) r) (== '(1) l)
       (splito n^ r^ '() h)))
    ((fresh (b n^ a r^ l^)
       (== `(,b . ,n^) n)
       (== `(,a . ,r^) r)
       (== `(,b . ,l^) l)
       (poso l^)
       (splito n^ r^ l^ h)))))

(defrel (/o n m q r)
  (conde
    ((== '() q) (== r n) (<o n m))
    ((== '(1) q) (=lo m n) (pluso r m n)
     (<o r m))
    ((poso q) (<lo m n) (<o r m)
     (n-wider-than-mo n m q r))))

(defrel (n-wider-than-mo n m q r)
  (fresh (nh nl qh ql)
    (fresh (mql mrql rr rh)
      (splito n r nl nh)
      (splito q r ql qh)
      (conde
        ((== '() nh)
         (== '() qh)
         (minuso nl r mql)
         (*o m ql mql))
        ((poso nh)
         (*o m ql mql)
         (pluso r mql mrql)
         (minuso mrql nl rr)
         (splito rr r '() rh)
         (/o nh m qh rh))))))

(defrel (logo n b q r)
  (conde
    ((== '() q) (<=o n b)
     (pluso r '(1) n))
    ((== '(1) q) (>1o b) (=lo n b)
     (pluso r b n))
    ((== '(1) b) (poso q)
     (pluso r '(1) n))
    ((== '() b) (poso q) (== r n))
    ((== '(0 1) b)
     (fresh (a ad dd)
       (poso dd)
       (== `(,a ,ad . ,dd) n)
       (exp2o n '() q)
       (fresh (s)
         (splito n dd r s))))
    ((<=o '(1 1) b) (<lo b n)
     (base-three-or-moreo n b q r))))

(defrel (exp2o n b q)
  (conde
    ((== '(1) n) (== '() q))
    ((>1o n) (== '(1) q)
     (fresh (s)
       (splito n b s '(1))))
    ((fresh (q1 b2)
       (== `(0 . ,q1) q) (poso q1)
       (<lo b n)
       (appendo b `(1 . ,b) b2)
       (exp2o n b2 q1)))
    ((fresh (q1 nh b2 s)
       (== `(1 . ,q1) q) (poso q1)
       (poso nh)
       (splito n b s nh)
       (appendo b `(1 . ,b) b2)
       (exp2o nh b2 q1)))))

(defrel (base-three-or-moreo n b q r)
  (fresh (bw1 bw nw nw1 ql1 ql s)
    (exp2o b '() bw1)
    (pluso bw1 '(1) bw)
    (<lo q n)
    (fresh (q1 bwq1)
      (pluso q '(1) q1)
      (*o bw q1 bwq1)
      (<o nw1 bwq1))
    (exp2o n '() nw1)
    (pluso nw1 '(1) nw)
    (/o nw bw ql1 s)
    (pluso ql '(1) ql1)
    (<=lo ql q)
    (fresh (bql qh s qdh qd)
      (repeated-mulo b ql bql)
      (/o nw bw1 qh s)
      (pluso ql qdh qh)
      (pluso ql qd q)
      (<=o qd qdh)
      (fresh (bqd bq1 bq)
        (repeated-mulo b qd bqd)
        (*o bql bqd bq)
        (*o b bq bq1)
        (pluso bq r n)
        (<o n bq1)))))

(defrel (repeated-mulo n q nq)
  (conde
    ((poso n) (== '() q) (== '(1) nq))
    ((== '(1) q) (== n nq))
    ((>1o q)
     (fresh (q1 nq1)
       (pluso q1 '(1) q)
       (repeated-mulo n q1 nq1)
       (*o nq1 n nq)))))

(defrel (expo b q n)
  (logo n b q '()))

;;; FRAMES

;;; EXERCISES

;; define addition using conde, ==, <o, and /o
;; (/o n m q r) succeeds if n = m*q + r where r < m
;; (+o n m r) succeeds if r = n + m
;;                     if r = n*1 + m
;; if n > m, then 1 <= (n + m) / n < 2
;; in other words, the quotient of (n + m)/n is 1
;;                 the remainder of (n+m)/n is m
;; (+o n m r) succeeds if n > m and (/o r n '(1) m)
(defrel (+o n m r)
  (conde
   ;; if m is zero, r = n
   [(== '() m) (== r n)]
   ;; if n and m are equal and positive, r = 2*m
   [(poso m) (== n m) (== r `(0 . ,m))]
   ;; if m > 0 and m < n, then
   ;; r/n = n*1 + m
   [(poso m) (<o m n)
             (/o r n '(1) m)]
   ;; if m > 0 and m > n, then
   ;; r/m = m*1 + n
   [(poso m) (poso n) (<o n m)
             (/o r m '(1) n)]))

;; the above doesn't seem to work quite well
;; (run* (x y) (+o x y (build-num 3)))
;; hangs. TODO