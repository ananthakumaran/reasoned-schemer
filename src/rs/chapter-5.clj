(ns rs.chapter-1
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.pprint :as pp]))

(run* [x]
  (appendo '(cake)
           '(tastes yummy)
           x))
;; => ((cake tastes yummy))

(run* [x]
  (fresh [y]
    (appendo (list 'cake 'with 'ice y)
             '(tastes yummy)
             x)))
;; => ((cake with ice _0 tastes yummy))

(run* [x]
  (fresh [y]
    (appendo '(cake with ice creaam)
             y
             x)))
;; => ((cake with ice creaam . _0))

(run 1 [x]
  (fresh [y]
    (appendo (llist 'cake 'with 'ice y) '(d t) x)))
;; => ((cake with ice d t))

(run 5 [x]
  (fresh [y]
    (appendo (llist 'cake 'with 'ice y) '(d t) x)))
;; => ((cake with ice d t)
;;     (cake with ice _0 d t)
;;     (cake with ice _0 _1 d t)
;;     (cake with ice _0 _1 _2 d t)
;;     (cake with ice _0 _1 _2 _3 d t))


(run 5 [y]
  (fresh [x]
    (appendo (llist 'cake 'with 'ice y) '(d t) x)))
;; => (() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3))


(run 5 [x]
  (fresh [y]
    (appendo
     (llist 'cake 'with 'ice y)
     (llist 'd 't y)
     x)))
;; => ((cake with ice d t)
;;     (cake with ice _0 d t _0)
;;     (cake with ice _0 _1 d t _0 _1)
;;     (cake with ice _0 _1 _2 d t _0 _1 _2)
;;     (cake with ice _0 _1 _2 _3 d t _0 _1 _2 _3))

(run* [x]
  (fresh [z]
    (appendo
     '(cake with ice creaam)
     (llist 'd 't z)
     x)))
;; => ((cake with ice creaam d t . _0))


(run 6 [x]
  (fresh [y]
    (appendo x y '(cake with ice d t))))
;; => (()
;;     (cake)
;;     (cake with)
;;     (cake with ice)
;;     (cake with ice d)
;;     (cake with ice d t))

(run 6 [y]
  (fresh [x]
    (appendo x y '(cake with ice d t))))
;; => ((cake with ice d t) (with ice d t) (ice d t) (d t) (t) ())

(run 6 [r]
  (fresh [x y]
    (appendo x y '(cake with ice d t))
    (== (list x y) r)))
;; => ((() (cake with ice d t))
;;     ((cake) (with ice d t))
;;     ((cake with) (ice d t))
;;     ((cake with ice) (d t))
;;     ((cake with ice d) (t))
;;     ((cake with ice d t) ()))


(run 7 [r]
  (fresh [x y]
    (appendo x y '(cake with ice d t))
    (== (list x y) r)))
;; => ((() (cake with ice d t))
;;     ((cake) (with ice d t))
;;     ((cake with) (ice d t))
;;     ((cake with ice) (d t))
;;     ((cake with ice d) (t))
;;     ((cake with ice d t) ()))

(run 7 [x]
  (fresh [y z]
    (appendo x y z)))
;; => (()
;;     (_0)
;;     (_0 _1)
;;     (_0 _1 _2)
;;     (_0 _1 _2 _3)
;;     (_0 _1 _2 _3 _4)
;;     (_0 _1 _2 _3 _4 _5))


(run 7 [y]
  (fresh [x z]
    (appendo x y z)))
;; => (_0 _0 _0 _0 _0 _0 _0)

(run 7 [z]
  (fresh [x y]
    (appendo x y z)))
;; => (_0
;;     (_0 . _1)
;;     (_0 _1 . _2)
;;     (_0 _1 _2 . _3)
;;     (_0 _1 _2 _3 . _4)
;;     (_0 _1 _2 _3 _4 . _5)
;;     (_0 _1 _2 _3 _4 _5 . _6))


(run 7 [r]
  (fresh [x y z]
    (appendo x y z)
    (== (list x y z) r)))
;; => ((() _0 _0)
;;     ((_0) _1 (_0 . _1))
;;     ((_0 _1) _2 (_0 _1 . _2))
;;     ((_0 _1 _2) _3 (_0 _1 _2 . _3))
;;     ((_0 _1 _2 _3) _4 (_0 _1 _2 _3 . _4))
;;     ((_0 _1 _2 _3 _4) _5 (_0 _1 _2 _3 _4 . _5))
;;     ((_0 _1 _2 _3 _4 _5) _6 (_0 _1 _2 _3 _4 _5 . _6)))


(defn swappendo [l s out]
  (conde
   [s# (fresh [a d res]
         (conso a d l)
         (conso a res out)
         (swappendo d s res))
    (emptyo l) (== s out)]))

(defn pairo [p]
  (fresh [a d]
    (conso a d p)))

(defn unwrapo [x out]
  (conde
   [(pairo x) (fresh [a]
                (firsto x a)
                (unwrapo a out))]
   [s# (== x out)]))

(run* [x]
  (unwrapo '(((pizza))) x))
;; => ((((pizza))) ((pizza)) (pizza) pizza)


(defn unwrapo [x out]
  (conde
   [s# (== x out)]
   [s# (fresh [a]
         (firsto x a)
         (unwrapo a out))]))

(run 5 [x]
  (unwrapo x 'pizza))
;; => (pizza
;;     (pizza . _0)
;;     ((pizza . _0) . _1)
;;     (((pizza . _0) . _1) . _2)
;;     ((((pizza . _0) . _1) . _2) . _3))


(run 5 [x]
  (unwrapo x '((pizza))))
;; => (((pizza))
;;     (clojure.lang.LazySeq@309e4e91 . _0)
;;     ((clojure.lang.LazySeq@309e4e91 . _0) . _1)
;;     (((clojure.lang.LazySeq@309e4e91 . _0) . _1) . _2)
;;     ((((clojure.lang.LazySeq@309e4e91 . _0) . _1) . _2) . _3))
;; not sure if there is any nesting limit on pretty print

(run 5 [x]
  (unwrapo (list (list x)) 'pizza))
;; => (pizza
;;     (pizza . _0)
;;     ((pizza . _0) . _1)
;;     (((pizza . _0) . _1) . _2)
;;     ((((pizza . _0) . _1) . _2) . _3))

(defn flatteno [s out]
  (conde
   [(emptyo s) (== '() out)]
   [(pairo s) (fresh [a d a-res d-res]
                (conso a d s)
                (flatteno a a-res)
                (flatteno d d-res)
                (appendo a-res d-res out))]
   [s# (conso s '() out)]))

(run 1 [x]
  (flatteno '((a b) c) x))
;; => ((((a b) c)))
;; because of condi


(run 1 [x]
  (flatteno '(a (b c)) x))
;; => (((a (b c))))
;; because of condi

(run* [x]
  (flatteno '(a) x))
;; => (((a)) (a) (a ()))


(run* [x]
  (flatteno '((a)) x))
;; => ((((a))) ((a)) ((a) ()) (a) (a ()) (a ()) (a () ()))


(run* [x]
  (flatteno '(((a))) x))
;; => (((((a))))
;;     (((a)))
;;     (((a)) ())
;;     ((a))
;;     ((a) ())
;;     ((a) ())
;;     ((a) () ())
;;     (a)
;;     (a ())
;;     (a ())
;;     (a () ())
;;     (a ())
;;     (a () ())
;;     (a () ())
;;     (a () () ()))


(run* [x]
  (flatteno '((a b) c) x))
;; => ((((a b) c))
;;     ((a b) (c))
;;     ((a b) c)
;;     (a (b) (c))
;;     ((a b) c ())
;;     (a (b) c)
;;     (a (b) c ())
;;     (a b (c))
;;     (a b () (c))
;;     (a b c)
;;     (a b c ())
;;     (a b () c)
;;     (a b () c ()))


(defn flattenrevo [s out]
  (conde
   [s# (conso s '() out)]
   [(emptyo s) (== '() out)]
   [s# (fresh [a d a-res d-res]
                (conso a d s)
                (flatteno a a-res)
                (flatteno d d-res)
                (appendo a-res d-res out))]))

(run* [x]
  (flattenrevo '((a b) c) x))
;; => ((((a b) c))
;;     ((a b) (c))
;;     ((a b) c)
;;     (a (b) (c))
;;     ((a b) c ())
;;     (a (b) c)
;;     (a (b) c ())
;;     (a b (c))
;;     (a b () (c))
;;     (a b c)
;;     (a b c ())
;;     (a b () c)
;;     (a b () c ()))


(comment
  (reverse
   (run* [x]
     (flattenrevo x '((a b) c)))))

(run 2 [x]
  (flattenrevo x '(a b c)))
;; => ((a b . c) (clojure.lang.LazySeq@1 a b . c))
