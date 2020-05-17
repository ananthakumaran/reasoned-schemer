(ns rs.chapter-8
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]))

(defn bit-xoro [x y r]
  (conde
   [(== 0 x) (== 0 y) (== 0 r)]
   [(== 1 x) (== 0 y) (== 1 r)]
   [(== 0 x) (== 1 y) (== 1 r)]
   [(== 1 x) (== 1 y) (== 0 r)]))

(defn bit-ando [x y r]
  (conde
   [(== 0 x) (== 0 y) (== 0 r)]
   [(== 1 x) (== 0 y) (== 0 r)]
   [(== 0 x) (== 1 y) (== 0 r)]
   [(== 1 x) (== 1 y) (== 1 r)]))

(defn half-addero [x y r c]
  (all
   (bit-xoro x y r)
   (bit-ando x y c)))

(defn full-addero [b x y r c]
  (fresh [w xy wz]
    (half-addero x y w xy)
    (half-addero w b r wz)
    (bit-xoro xy wz c)))

(defn build-num [x]
  (if (= x 0)
    '()
    (cons (rem x 2) (build-num (quot x 2)))))

(defn poso [n]
  (fresh [a d]
    (== (lcons a d) n)))

(defn >1o [n]
  (fresh [a ad dd]
    (== (llist a ad dd) n)))

(declare addero)

(defn gen-addero [d n m r]
  (fresh [a b c e x y z]
    (== (lcons a x) n)
    (== (lcons b y) m) (poso y)
    (== (lcons c z) r) (poso z)
    (all
     (full-addero d a b c e)
     (addero e x y z))))

(defn addero [d n m r]
  (conde
   [(== 0 d) (== '() m) (== n r)]
   [(== 0 d) (== '() n) (== m r) (poso m)]
   [(== 1 d) (== '() m) (addero 0 n '(1) r)]
   [(== 1 d) (== '() n) (poso m) (addero 0 '(1) m r)]
   [(== '(1) n) (== '(1) m) (fresh [a c]
                              (== (list a c) r)
                              (full-addero d 1 1 a c))]
   [(== '(1) n) (gen-addero d n m r)]
   [(== '(1) m) (>1o n) (>1o r) (addero d '(1) n r)]
   [(>1o n) (gen-addero d n m r)]
   [s# u#]))

(defn +o [n m k]
  (addero 0 n m k))

(defn -o [n m k]
  (addero 0 m k n))

(defn bound-*o [q p n m]
  s#)

(declare *o)

(defn odd-*o [x n m p]
  (fresh [q]
    (bound-*o q p n m)
    (*o x m q)
    (+o (lcons 0 q) m p)))

(defn *o [n m p]
  (conde
   [(== '() n) (== '() p)]
   [(poso n) (== '() m) (== '() p)]
   [(== '(1) n) (poso m) (== m p)]
   [(>1o n) (== '(1) m) (== n p)]
   [(fresh [x z]
      (== (lcons 0 x) n) (poso x)
      (== (lcons 0 z) p) (poso z)
      (>1o m)
      (*o x m z))]
   [(fresh [x y]
      (== (lcons 1 x) n) (poso x)
      (== (lcons 0 y) m) (poso y)
      (*o m n p))]
   [(fresh [x y]
      (== (lcons 1 x) n) (poso x)
      (== (lcons 1 y) m) (poso y)
      (odd-*o x n m p))]
   [s# u#]))

(run 34 [t]
  (fresh [x y r]
    (*o x y r)
    (== (list x y r) t)))
;; => ((() _0 ())
;;     ((_0 . _1) () ())
;;     ((1) (_0 . _1) (_0 . _1))
;;     ((_0 _1 . _2) (1) (_0 _1 . _2))
;;     ((0 1) (_0 _1 . _2) (0 _0 _1 . _2))
;;     ((0 0 1) (_0 _1 . _2) (0 0 _0 _1 . _2))
;;     ((1 _0 . _1) (0 1) (0 1 _0 . _1))
;;     ((0 0 0 1) (_0 _1 . _2) (0 0 0 _0 _1 . _2))
;;     ((1 _0 . _1) (0 0 1) (0 0 1 _0 . _1))
;;     ((0 1 _0 . _1) (0 1) (0 0 1 _0 . _1))
;;     ((0 0 0 0 1) (_0 _1 . _2) (0 0 0 0 _0 _1 . _2))
;;     ((1 _0 . _1) (0 0 0 1) (0 0 0 1 _0 . _1))
;;     ((0 1 _0 . _1) (0 0 1) (0 0 0 1 _0 . _1))
;;     ((0 0 1 _0 . _1) (0 1) (0 0 0 1 _0 . _1))
;;     ((0 0 0 0 0 1) (_0 _1 . _2) (0 0 0 0 0 _0 _1 . _2))
;;     ((1 _0 . _1) (0 0 0 0 1) (0 0 0 0 1 _0 . _1))
;;     ((0 1 _0 . _1) (0 0 0 1) (0 0 0 0 1 _0 . _1))
;;     ((0 0 1 _0 . _1) (0 0 1) (0 0 0 0 1 _0 . _1))
;;     ((0 0 0 1 _0 . _1) (0 1) (0 0 0 0 1 _0 . _1))
;;     ((1 1) (1 1) (1 0 0 1))
;;     ((0 0 0 0 0 0 1) (_0 _1 . _2) (0 0 0 0 0 0 _0 _1 . _2))
;;     ((1 _0 . _1) (0 0 0 0 0 1) (0 0 0 0 0 1 _0 . _1))
;;     ((0 1 _0 . _1) (0 0 0 0 1) (0 0 0 0 0 1 _0 . _1))
;;     ((1 0 1) (1 1) (1 1 1 1))
;;     ((0 0 1 _0 . _1) (0 0 0 1) (0 0 0 0 0 1 _0 . _1))
;;     ((0 0 0 1 _0 . _1) (0 0 1) (0 0 0 0 0 1 _0 . _1))
;;     ((1 1) (1 0 1) (1 1 1 1))
;;     ((0 0 0 0 1 _0 . _1) (0 1) (0 0 0 0 0 1 _0 . _1))
;;     ((0 1 1) (1 1) (0 1 0 0 1))
;;     ((1 1) (0 1 1) (0 1 0 0 1))
;;     ((1 1) (1 1 1) (1 0 1 0 1))
;;     ((0 0 0 0 0 0 0 1) (_0 _1 . _2) (0 0 0 0 0 0 0 _0 _1 . _2))
;;     ((1 _0 . _1) (0 0 0 0 0 0 1) (0 0 0 0 0 0 1 _0 . _1))
;;     ((0 1 _0 . _1) (0 0 0 0 0 1) (0 0 0 0 0 0 1 _0 . _1)))

(run* [p]
  (*o '(0 1) '(0 0 1) p))
;; => ((0 0 0 1))


(run 1 [t]
  (fresh [n m]
    (*o n m '(1))
    (== (list n m) t)))
;; => (((1) (1)))

(defn pairo [p]
  (fresh [a d]
    (conso a d p)))

(defn bound-*o [q p n m]
  (conde
   [(emptyo q) (pairo p)]
   [s# (fresh [x y z]
         (resto q x)
         (resto p y)
         (conde
          [(emptyo n) (resto m z) (bound-*o x y z '())]
          [s# (resto n z) (bound-*o x y z m)]))]))

(run 2 [t]
  (fresh [n m]
    (*o n m '(1))
    (== (list n m) t)))
;; => (((1) (1)))

(run* [p]
  (*o '(1 1 1) '(1 1 1 1 1 1) p))
;; => ((1 0 0 1 1 1 0 1 1))

(defn =lo [n m]
  (conde
   [(== '() n) (== '() m)]
   [(== '(1) n) (== '(1) m)]
   [s# (fresh [a x b y]
         (== (lcons a x) n) (poso x)
         (== (lcons b y) m) (poso y)
         (=lo x y))]))

(run* [t]
  (fresh [w x y]
    (=lo (llist 1 w x y) '(0 1 1 0 1))
    (== (list w x y) t)))
;; => ((_0 _1 (_2 1)))

(run* [b]
  (=lo '(1) (list b)))
;; => (1)


(run* [n]
  (=lo (llist 1 0 1 n) '(0 1 1 0 1)))
;; => ((_0 1))

(run 5 [t]
  (fresh [y z]
    (=lo (lcons 1 y) (lcons 1 z))
    (== (list y z) t)))
;; => ((() ())
;;     ((1) (1))
;;     ((_0 1) (_1 1))
;;     ((_0 _1 1) (_2 _3 1))
;;     ((_0 _1 _2 1) (_3 _4 _5 1)))

(run 5 [t]
  (fresh [y z]
    (=lo (lcons 1 y) (lcons 0 z))
    (== (list y z) t)))
;; => (((1) (1))
;;     ((_0 1) (_1 1))
;;     ((_0 _1 1) (_2 _3 1))
;;     ((_0 _1 _2 1) (_3 _4 _5 1))
;;     ((_0 _1 _2 _3 1) (_4 _5 _6 _7 1)))

(run 5 [t]
  (fresh [y z]
    (=lo (lcons 1 y) (llist 0 1 1 0 1 z))
    (== (list y z) t)))
;; => (((_0 _1 _2 1) ())
;;     ((_0 _1 _2 _3 1) (1))
;;     ((_0 _1 _2 _3 _4 1) (_5 1))
;;     ((_0 _1 _2 _3 _4 _5 1) (_6 _7 1))
;;     ((_0 _1 _2 _3 _4 _5 _6 1) (_7 _8 _9 1)))

(defn <lo [n m]
  (conde
   [(== '() n) (poso m)]
   [(== '(1) n) (>1o m)]
   [s# (fresh [a x b y]
         (== (lcons a x) n) (poso x)
         (== (lcons b y) m) (poso y)
         (<lo x y))]))

(run 8 [t]
  (fresh [y z]
    (<lo (lcons 1 y) (llist 0 1 1 0 1 z))
    (== (list y z) t)))
;; => ((() _0)
;;     ((1) _0)
;;     ((_0 1) _1)
;;     ((_0 _1 1) _2)
;;     ((_0 _1 _2 1) (_3 . _4))
;;     ((_0 _1 _2 _3 1) (_4 _5 . _6))
;;     ((_0 _1 _2 _3 _4 1) (_5 _6 _7 . _8))
;;     ((_0 _1 _2 _3 _4 _5 1) (_6 _7 _8 _9 . _10)))

(defn <=lo [n m]
  (conde
   [(=lo n m) s#]
   [(<lo n m) s#]
   [s# u#]))

(run 8 [t]
  (fresh [n m]
    (<=lo n m)
    (== (list n m) t)))
;; => ((() ())
;;     ((1) (1))
;;     (() (_0 . _1))
;;     ((1) (_0 _1 . _2))
;;     ((_0 1) (_1 1))
;;     ((_0 1) (_1 _2 _3 . _4))
;;     ((_0 _1 1) (_2 _3 1))
;;     ((_0 _1 1) (_2 _3 _4 _5 . _6)))

(run 1 [t]
  (fresh [n m]
    (<=lo n m)
    (*o n '(0 1) m)
    (== (list n m) t)))
;; => ((() ()))

(run 2 [t]
  (fresh [n m]
    (<=lo n m)
    (*o n '(0 1) m)
    (== (list n m) t)))
;; => ((() ()) ((1) (0 1)))

(run 10 [t]
  (fresh [n m]
    (<=lo n m)
    (*o n '(0 1) m)
    (== (list n m) t)))
;; => ((() ())
;;     ((1) (0 1))
;;     ((0 1) (0 0 1))
;;     ((1 1) (0 1 1))
;;     ((1 _0 1) (0 1 _0 1))
;;     ((0 0 1) (0 0 0 1))
;;     ((0 1 1) (0 0 1 1))
;;     ((1 _0 _1 1) (0 1 _0 _1 1))
;;     ((0 1 _0 1) (0 0 1 _0 1))
;;     ((0 0 0 1) (0 0 0 0 1)))

(run 15 [t]
  (fresh [n m]
    (<=lo n m)
    (== (list n m) t)))
;; => ((() ())
;;     ((1) (1))
;;     (() (_0 . _1))
;;     ((1) (_0 _1 . _2))
;;     ((_0 1) (_1 1))
;;     ((_0 1) (_1 _2 _3 . _4))
;;     ((_0 _1 1) (_2 _3 1))
;;     ((_0 _1 1) (_2 _3 _4 _5 . _6))
;;     ((_0 _1 _2 1) (_3 _4 _5 1))
;;     ((_0 _1 _2 _3 1) (_4 _5 _6 _7 1))
;;     ((_0 _1 _2 1) (_3 _4 _5 _6 _7 . _8))
;;     ((_0 _1 _2 _3 _4 1) (_5 _6 _7 _8 _9 1))
;;     ((_0 _1 _2 _3 1) (_4 _5 _6 _7 _8 _9 . _10))
;;     ((_0 _1 _2 _3 _4 _5 1) (_6 _7 _8 _9 _10 _11 1))
;;     ((_0 _1 _2 _3 _4 1) (_5 _6 _7 _8 _9 _10 _11 . _12)))

(defn <o [n m]
  (conde
   [(<lo n m) s#]
   [(=lo n m) (fresh [x]
                (poso x)
                (+o n x m))]
   [s# u#]))

(defn <=o [n m]
  (conde
   [(== n m) s#]
   [(<o n m) s#]
   [s# u#]))

(run* [q]
  (<o '(1 0 1) '(1 1 1))
  (== true q))
;; => (true)

(run* [q]
  (<o '(1 1 1) '(1 0 1))
  (== true q))
;; => ()

(run* [q]
  (<o '(1 0 1) '(1 0 1))
  (== true q))
;; => ()

(run 6 [n]
  (<o n '(1 0 1)))
;; => (() (1) (_0 1) (0 0 1))

(run 6 [m]
  (<o '(1 0 1) m))
;; => ((_0 _1 _2 _3 . _4) (0 1 1) (1 1 1))

(defn %o [n m q r]
  (conde
   [(== '() q) (== n r) (<o n m)]
   [(== '(1) q) (== '() r) (== n m) (<o r m)]
   [(<o m n) (<o r m) (fresh [mq]
                        (<=lo mq n)
                        (*o m q mq)
                        (+o mq r n))]
   [s# u#]))

(run 15 [t]
  (fresh [n m q r]
    (%o n m q r)
    (== (list n m q r) t)))
;; => ((() (_0 . _1) () ())
;;     ((1) (_0 _1 . _2) () (1))
;;     ((_0 . _1) (_0 . _1) (1) ())
;;     ((_0 1) (_1 _2 _3 . _4) () (_0 1))
;;     ((_0 _1 1) (_2 _3 _4 _5 . _6) () (_0 _1 1))
;;     ((_0 _1 _2 1) (_3 _4 _5 _6 _7 . _8) () (_0 _1 _2 1))
;;     ((_0 _1 _2 _3 1) (_4 _5 _6 _7 _8 _9 . _10) () (_0 _1 _2 _3 1))
;;     ((_0 _1 _2 _3 _4 1)
;;      (_5 _6 _7 _8 _9 _10 _11 . _12)
;;      ()
;;      (_0 _1 _2 _3 _4 1))
;;     ((_0 _1 _2 _3 _4 _5 1)
;;      (_6 _7 _8 _9 _10 _11 _12 _13 . _14)
;;      ()
;;      (_0 _1 _2 _3 _4 _5 1))
;;     ((_0 _1 _2 _3 _4 _5 _6 1)
;;      (_7 _8 _9 _10 _11 _12 _13 _14 _15 . _16)
;;      ()
;;      (_0 _1 _2 _3 _4 _5 _6 1))
;;     ((_0 1) (1) (_0 1) ())
;;     ((_0 _1 _2 _3 _4 _5 _6 _7 1)
;;      (_8 _9 _10 _11 _12 _13 _14 _15 _16 _17 . _18)
;;      ()
;;      (_0 _1 _2 _3 _4 _5 _6 _7 1))
;;     ((_0 _1 _2 _3 _4 _5 _6 _7 _8 1)
;;      (_9 _10 _11 _12 _13 _14 _15 _16 _17 _18 _19 . _20)
;;      ()
;;      (_0 _1 _2 _3 _4 _5 _6 _7 _8 1))
;;     ((_0 _1 _2 _3 _4 _5 _6 _7 _8 _9 1)
;;      (_10 _11 _12 _13 _14 _15 _16 _17 _18 _19 _20 _21 . _22)
;;      ()
;;      (_0 _1 _2 _3 _4 _5 _6 _7 _8 _9 1))
;;     ((_0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 1)
;;      (_11 _12 _13 _14 _15 _16 _17 _18 _19 _20 _21 _22 _23 . _24)
;;      ()
;;      (_0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 1)))

(run* [m]
  (fresh [r]
    (%o '(1 0 1) m '(1 1 1) r)))
;; => ()

(defn splito [n r l h]
  (conde
   [(== '() n) (== '() h) (== '() l)]
   [(fresh [b np]
      (== (llist 0 b np) n)
      (== '() r)
      (== (llist b np) h)
      (== '() l))]
   [(fresh [np]
      (== (llist 1 np) n)
      (== '() r)
      (== np h)
      (== '(1) l))]
   [(fresh [b np a rp]
      (== (llist 0 b np) n)
      (== (llist a rp) r)
      (== '() l)
      (splito (llist b np) rp '() h))]
   [(fresh [np a rp]
      (== (llist 1 np) n)
      (== (llist a rp) r)
      (== '(1) l)
      (splito np rp '() h))]
   [(fresh [b np a rp lp]
      (== (llist b np) n)
      (== (llist a rp) r)
      (== (llist b lp) l)
      (poso lp)
      (splito np rp lp h))]
   [s# u#]))

(defn %o [n m q r]
  (conde
   [(== r n) (== '() q) (<o n m)]
   [(== '(1) q) (=lo n m) (+o r m n) (<o r m)]
   [s# (all
        (<lo m n)
        (<o r m)
        (poso q)
        (fresh [nh nl qh ql qlm qlmr rr rh]
          (all
           (splito n r nl nh)
           (splito q r ql qh)
           (conde
            [(== '() nh)
             (== '() qh)
             (-o nl r qlm)
             (*o ql m qlm)]
            [s# (all
                 (poso nh)
                 (*o ql m qlm)
                 (+o qlm r qlmr)
                 (-o qlmr nl rr)
                 (splito rr r '() rh)
                 (%o nh m qh rh))]))))]))

(run 3 [t]
  (fresh [y z]
    (%o (llist 1 0 y) '(0 1) z '())
    (== (list y z) t)))
;; => ()

(defn exp2o [n b q]
  (conde
   [(== '(1) n) (== '() q)]
   [(>1o n) (== '(1) q)
    (fresh [s]
      (splito n b s '(1)))]
   [(fresh [q1 b2]
      (all
       (== (llist 0 q1) q)
       (poso q1)
       (<lo b n)
       (appendo b (llist 1 b) b2)
       (exp2o n b2 q1)))]
   [(fresh [q1 nh b2 s]
      (all
       (== (llist 1 q1) q)
       (poso q1)
       (poso nh)
       (splito n b s nh)
       (appendo b (llist 1 b) b2)
       (exp2o nh b2 q1)))]
   [s# u#]))

(defn repeated-mulo [n q nq]
  (conde
   [(poso n) (== '() q) (== '(1) nq)]
   [(== '(1) q) (== n nq)]
   [(>1o q) (fresh [q1 nq1]
              (+o q1 '(1) q)
              (repeated-mulo n q1 nq1)
              (*o nq1 n nq))]
   [s# u#]))

(defn logo [n b q r]
  (conde
    [(== '(1) n) (poso b) (== '() q) (== '() r)]
    [(== '() q) (<o n b) (+o r '(1) n)]
    [(== '(1) q) (>1o b) (=lo n b) (+o r b n)]
    [(== '(1) b) (poso q) (+o r '(1) n)]
    [(== '() b) (poso q) (== r n)]
    [(== '(0 1) b)
     (fresh [a ad dd]
       (poso dd)
       (== (llist a ad dd) n)
       (exp2o n '() q)
       (fresh [s]
         (splito n dd r s)))]
    [(fresh [a ad add ddd]
       (conde
        [(== '(1 1) b)]
        [s# (== (llist a ad add ddd) b)]))
     (<lo b n)
     (fresh [bw1 bw nw nw1 ql1 ql s]
       (exp2o b '() bw1)
       (+o bw1 '(1) bw)
       (<lo q n)
       (fresh [q1 bwq1]
         (+o q '(1) q1 )
         (*o bw q1 bwq1)
         (<o nw1 bwq1)
         (exp2o n '() nw1)
         (+o nw1 '(1) nw)
         (%o nw bw ql1 s)
         (+o ql '(1) ql1))
       (conde
        [(== q ql)]
        [s# (<lo ql q)])
       (fresh [bql qh s qdh qd]
         (repeated-mulo b ql bql)
         (%o nw bw1 qh s)
         (+o ql qdh qh)
         (+o ql qd q)
         (conde
          [(== qd qdh)]
          [s# (<o qd qdh)])
         (fresh [bqd bq1 bq]
           (repeated-mulo b qd bqd)
           (*o bql bqd bq)
           (*o b bq bq1)
           (+o bq r n)
           (<o n bq1))))]
    [s# u#]))

(run* [r]
  (logo '(0 1 1 1) '(0 1) '(1 1) r))
;; => ((0 1 1))

(run 8 [s]
  (fresh [b q r]
    (logo '(0 0 1 0 0 0 1) b q r)
    (>1o q)
    (== (llist b q r) s)))
;; => ((() (_0 _1 . _2) 0 0 1 0 0 0 1)
;;     ((1) (_0 _1 . _2) 1 1 0 0 0 0 1)
;;     ((0 1) (0 1 1) 0 0 1)
;;     ((0 0 1) (1 1) 0 0 1)
;;     ((0 0 0 1) (0 1) 0 0 1)
;;     ((1 0 1) (0 1) 1 1 0 1 0 1)
;;     ((0 1 1) (0 1) 0 0 0 0 0 1)
;;     ((1 1 1) (0 1) 1 1 0 0 1))

(defn expo [b q n]
  (logo n b q '()))

(run* [t]
  (expo '(1 1) '(1 0 1) t))
;; => ((1 1 0 0 1 1 1 1))
