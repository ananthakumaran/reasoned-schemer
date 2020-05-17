(ns rs.chapter-3
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]))

(defn pairo [p]
  (fresh [a d]
    (conso a d p)))

(defn listo [p]
  (conde
   [(emptyo p) s#]
   [(pairo p) (fresh [l]
                (resto p l)
                (listo l))]
   [s# u#]))

(run* [x]
  (listo (list 'a 'b x 'd)))
;; => (_0)

(run 1 [x]
  (listo (lcons 'a (lcons 'b (lcons 'c x)))))
;; => (())


(run 5 [x]
  (listo (lcons 'a (lcons 'b (lcons 'c x)))))
;; => (() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3))

(defn lolo [l]
  (conde
   [(emptyo l) s#]
   [(fresh [a]
      (firsto l a)
      (listo a))
    (fresh [d]
      (resto l d)
      (lolo d))]
   [s# u#]))

(run 1 [l]
  (lolo l))
;; => (())

(run* [q]
  (fresh [x y]
    (lolo (list '(a b) (list x 'c) (list 'd y)))
    (== true q)))
;; => (true)

(run 1 [q]
  (fresh [x]
    (lolo (lcons '(a b) x))
    (== true q)))
;; => (true)

(run 1 [x]
  (lolo (lcons '(a b) (lcons '(c d) x))))
;; => (())

(run 5 [x]
  (lolo (lcons '(a b) (lcons '(c d) x))))
;; => (() (()) ((_0)) (() ()) ((_0 _1)))

(defn twinso [s]
  (fresh [x y]
    (conso x y s)
    (conso x '() y)))

(run* [q]
  (twinso '(tofu tofu))
  (== true q))
;; => (true)

(run* [z]
  (twinso (list z 'tofu)))
;; => (tofu)

(defn twinso [s]
  (fresh [x]
    (== (list x x) s)))

(defn loto [l]
  (conde
   [(emptyo l) s#]
   [(fresh [a]
      (firsto l a)
      (twinso a))
    (fresh [d]
      (resto l d)
      (loto d))]
   [s# u#]))

(run 1 [z]
  (loto (lcons '(g g) z)))
;; => (())


(run 5 [z]
  (loto (lcons '(g g) z)))
;; => (()
;;     ((_0 _0))
;;     ((_0 _0) (_1 _1))
;;     ((_0 _0) (_1 _1) (_2 _2))
;;     ((_0 _0) (_1 _1) (_2 _2) (_3 _3)))

(run 5 [r]
  (fresh [w x y z]
    (loto (lcons '(g g) (lcons (list 'e w) (lcons (list x y) z))))
    (== (list w (list x y) z) r)))
;; => ((e (_0 _0) ())
;;     (e (_0 _0) ((_1 _1)))
;;     (e (_0 _0) ((_1 _1) (_2 _2)))
;;     (e (_0 _0) ((_1 _1) (_2 _2) (_3 _3)))
;;     (e (_0 _0) ((_1 _1) (_2 _2) (_3 _3) (_4 _4))))

(run 3 [out]
  (fresh [w x y z]
    (== (lcons '(g g) (lcons (list 'e w) (lcons (list x y) z))) out)
    (loto out)))
;; => (((g g) (e e) (_0 _0))
;;     ((g g) (e e) (_0 _0) (_1 _1))
;;     ((g g) (e e) (_0 _0) (_1 _1) (_2 _2)))


(defn listofo [predo l]
  (conde
   [(emptyo l) s#]
   [(fresh [a]
      (firsto l a)
      (predo a))
    (fresh [d]
      (resto l d)
      (listofo predo d))]
   [s# u#]))

(run 3 [out]
  (fresh [w x y z]
    (== (lcons '(g g) (lcons (list 'e w) (lcons (list x y) z))) out)
    (listofo twinso out)))
;; => (((g g) (e e) (_0 _0))
;;     ((g g) (e e) (_0 _0) (_1 _1))
;;     ((g g) (e e) (_0 _0) (_1 _1) (_2 _2)))

(defn loto [l]
  (listofo twinso l))

(run* [q]
  (membero 'olive '(virgin olive oil))
  (== true q))
;; => (true)

(run 1 [y]
  (membero y '(hummus with pita)))
;; => (hummus)

(run 1 [y]
  (membero y '(with pita)))
;; => (with)

(run 1 [y]
  (membero y '(pita)))
;; => (pita)

(run 1 [y]
  (membero y '()))
;; => ()

(run* [y]
  (membero y '(hummus with pita)))
;; => (hummus with pita)

(run* [x]
  (membero 'e (list 'pasta x 'fagioli)))
;; => (e)

(run 1 [x]
  (membero 'e (list 'pasta 'e x 'faioli)))
;; => (_0)

(run 1 [x]
  (membero 'e (list 'pasta x 'e 'faioli)))
;; => (e)

(run* [r]
  (fresh [x y]
    (membero 'e (list 'pasta x 'fagioli y))
    (== (list x y) r)))
;; => ((e _0) (_0 e))

(run 1 [l]
  (membero 'tofu l))
;; => ((tofu . _0))

(run 5 [l]
  (membero 'tofu l))
;; => ((tofu . _0)
;;     (_0 tofu . _1)
;;     (_0 _1 tofu . _2)
;;     (_0 _1 _2 tofu . _3)
;;     (_0 _1 _2 _3 tofu . _4))



(defn pmembero [x l]
  (conde
   [(emptyo l) u#]
   [(firsto l x) (resto l '())]
   [s# (fresh [d]
         (resto l d)
         (pmembero x d))]))

(run 5 [l]
  (pmembero 'tofu l))
;; => ((tofu) (_0 tofu) (_0 _1 tofu) (_0 _1 _2 tofu) (_0 _1 _2 _3 tofu))

(run* [q]
  (pmembero 'tofu '(a b tofu d tofu))
  (== true q))
;; => (true)

(defn pmembero [x l]
  (conde
   [(emptyo l) u#]
   [(firsto l x) (resto l '())]
   [(firsto l x) s#]
   [s# (fresh [d]
         (resto l d)
         (pmembero x d))]))

(run* [q]
  (pmembero 'tofu '(a b tofu d tofu))
  (== true q))
;; => (true true true)


(defn pmembero [x l]
  (conde
   [(emptyo l) u#]
   [(firsto l x) (resto l '())]
   [(firsto l x) (fresh [a d]
                   (resto l (lcons a d)))]
   [s# (fresh [d]
         (resto l d)
         (pmembero x d))]))

(run* [q]
  (pmembero 'tofu '(a b tofu d tofu))
  (== true q))
;; => (true true)


(run 12 [l]
  (pmembero 'tofu l))
;; => ((tofu)
;;     (tofu _0 . _1)
;;     (_0 tofu)
;;     (_0 tofu _1 . _2)
;;     (_0 _1 tofu)
;;     (_0 _1 tofu _2 . _3)
;;     (_0 _1 _2 tofu)
;;     (_0 _1 _2 tofu _3 . _4)
;;     (_0 _1 _2 _3 tofu)
;;     (_0 _1 _2 _3 tofu _4 . _5)
;;     (_0 _1 _2 _3 _4 tofu)
;;     (_0 _1 _2 _3 _4 tofu _5 . _6))


(defn pmembero [x l]
  (conde
   [(firsto l x) (fresh [a d]
                   (resto l (lcons a d)))]
   [(firsto l x) (resto l '())]
   [s# (fresh [d]
         (resto l d)
         (pmembero x d))]))

(run 12 [l]
  (pmembero 'tofu l))
;; => ((tofu _0 . _1)
;;     (tofu)
;;     (_0 tofu _1 . _2)
;;     (_0 tofu)
;;     (_0 _1 tofu _2 . _3)
;;     (_0 _1 tofu)
;;     (_0 _1 _2 tofu _3 . _4)
;;     (_0 _1 _2 tofu)
;;     (_0 _1 _2 _3 tofu _4 . _5)
;;     (_0 _1 _2 _3 tofu)
;;     (_0 _1 _2 _3 _4 tofu _5 . _6)
;;     (_0 _1 _2 _3 _4 tofu))

(defn first-value [l]
  (run 1 [y]
    (membero y l)))

(first-value '(pasta e fagioli))
;; => (pasta)

(defn memberrevo [x l]
  (conde
   [s# (fresh [d]
         (resto l d)
         (memberrevo x d))]
   [s# (firsto l x)]))

(run* [x]
  (memberrevo x '(pasta e fagioli)))
;; => (pasta e fagioli)

;; not same as book because conde might not execute in the order of
;; definition
