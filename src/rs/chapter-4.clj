(ns rs.chapter-4
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]))

(defn memo [x l out]
  (conde
   [(emptyo l) u#]
   [(firsto l x) (== l out)]
   [s# (fresh [d]
         (resto l d)
         (memo x d out))]))

(run 1 [out]
  (memo 'tofu '(a b tofu d tofu e) out))
;; => ((tofu d tofu e))

(run 1 [out]
  (fresh [x]
    (memo 'tofu (list 'a 'b x 'd 'tofu 'e) out)))
;; => ((tofu d tofu e))

(run* [r]
  (memo r '(a b tofu d tofu e) '(tofu d tofu e)))
;; => (tofu)

(run* [q]
  (memo 'tofu '(tofu e) '(tofu e))
  (== true q))
;; => (true)

(run* [q]
  (memo 'tofu '(tofu e) '(tofu))
  (== true q))
;; => ()

(run* [x]
  (memo 'tofu '(tofu e) (list x 'e)))
;; => (tofu)

(run* [x]
  (memo 'tofu '(tofu e) (list 'peas x)))
;; => ()

(run* [out]
  (fresh [x]
    (memo 'tofu (list 'a 'b x 'd 'tofu 'e) out)))
;; => ((tofu d tofu e) (tofu e))

(run 12 [z]
  (fresh [u]
    (memo 'tofu (llist 'a 'b 'tofu 'd 'tofu 'e z) u)))
;; => (_0
;;     _0
;;     (tofu . _0)
;;     (_0 tofu . _1)
;;     (_0 _1 tofu . _2)
;;     (_0 _1 _2 tofu . _3)
;;     (_0 _1 _2 _3 tofu . _4)
;;     (_0 _1 _2 _3 _4 tofu . _5)
;;     (_0 _1 _2 _3 _4 _5 tofu . _6)
;;     (_0 _1 _2 _3 _4 _5 _6 tofu . _7)
;;     (_0 _1 _2 _3 _4 _5 _6 _7 tofu . _8)
;;     (_0 _1 _2 _3 _4 _5 _6 _7 _8 tofu . _9))

(defn rembero1 [x l out]
  (conde
   [(emptyo l) (== '() out)]
   [(firsto l x) (resto l out)]
   [s# (fresh [res]
         (fresh [d]
           (resto l d)
           (rembero1 x d res))
         (fresh [a]
           (firsto l a)
           (conso a res out)))]))

(run 1 [out]
  (fresh [y]
    (rembero1 'peas (list 'a 'b y 'd 'peas 'e) out)))
;; => ((a b d peas e))

(run* [out]
  (fresh [y z]
    (rembero1 y (list 'a 'b y 'd z 'e) out)))
;; => ((b a d _0 e)
;;     (a b d _0 e)
;;     (a b d _0 e)
;;     (a b d _0 e)
;;     (a b _0 d e)
;;     (a b e d _0)
;;     (a b _0 d _1 e))

(run* [r]
  (fresh [y z]
    (rembero1 y (list y 'd z 'e) (list y 'd 'e))
    (== (list y z) r)))
;; => ((d d) (d d) (_0 _0) (e e))

(run 13 [w]
  (fresh [y z out]
    (rembero1 y (llist 'a 'b y 'd z w) out)))
;; => (_0
;;     _0
;;     _0
;;     _0
;;     _0
;;     ()
;;     (_0 . _1)
;;     (_0)
;;     (_0 _1 . _2)
;;     (_0 _1)
;;     (_0 _1 _2 . _3)
;;     (_0 _1 _2)
;;     (_0 _1 _2 _3 . _4))

(defn surpriseo [s]
  (rembero1 s '(a b c) '(a b c)))

(run* [r]
  (== 'd r)
  (surpriseo r))
;; => (d)

(run* [r]
  (surpriseo r))
;; => (_0)

(run* [r]
  (surpriseo r)
  (== 'b r))
;; => (b)
