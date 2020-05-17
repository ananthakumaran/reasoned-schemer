(ns rs.chapter-2
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]))

(let [x (fn [a] a)
      y 'c]
  (x y))
;; => c

(run* [r]
  (fresh [y x]
    (== (list x y) r)))
;; => ((_0 _1))

(run* [r]
  (fresh [v w]
    (== (let [x v
              y w]
          (list x y))
        r)))
;; => ((_0 _1))

(first '(grape raisin pear))
;; => grape

(first '(a c o r n))
;; => a

(run* [r]
  (firsto '(a c o r n) r))
;; => (a)

(run* [q]
  (firsto '(a c o r n) 'a)
  (== true q))
;; => (true)

(run* [r]
  (fresh [x y]
    (firsto (list r y) x)
    (== 'pear x)))
;; => (pear)

(run* [r]
  (fresh [x y]
    (firsto '(grape raisin pear) x)
    (firsto '((a) (b) (c)) y)
    (== (lcons x y) r)))
;; => ((grape a))

(run* [r]
  (fresh [v]
    (resto '(a c o r n) v)
    (firsto v r)))
;; => (c)

(run* [r]
  (fresh [x y]
    (resto '(grape raisin peer) x)
    (firsto '((a) (b) (c)) y)
    (== (lcons x y) r)))
;; => (((raisin peer) a))

(run* [q]
  (resto '(a c o r n) '(c o r n))
  (== true q))
;; => (true)


(run* [x]
  (resto '(c o r n) (list x 'r 'n)))
;; => (o)

(run* [l]
  (fresh [x]
    (resto l '(c o r n))
    (firsto l x)
    (== 'a x)))
;; => ((a c o r n))

(run* [l]
  (conso '(a b c) '(d e) l))
;; => (((a b c) d e))

(run* [x]
  (conso x '(a b c) '(d a b c)))
;; => (d)

(run* [r]
  (fresh [x y z]
    (== (list 'e 'a 'd x) r)
    (conso y (list 'a z 'c) r)))
;; => ((e a d c))

(run* [x]
  (conso x (list 'a x 'c) (list 'd 'a x 'c)))
;; => (d)

(run* [l]
  (fresh [x]
    (== (list 'd 'a x 'c) l)
    (conso x (list 'a x 'c) l)))
;; => ((d a d c))

(run* [l]
  (fresh [x]
    (conso x (list 'a x 'c) l)
    (== (list 'd 'a x 'c) l)))
;; => ((d a d c))

(run* [l]
  (fresh [d x y w s]
    (conso w '(a n s) s)
    (resto l s)
    (firsto l x)
    (== 'b x)
    (resto l d)
    (firsto d y)
    (== 'e y)))
;; => ((b e a n s))

(run* [q]
  (emptyo '(grape raisin pear))
  (== true q))
;; => ()

(run* [q]
  (emptyo '())
  (== true q))
;; => (true)

(run* [x]
  (emptyo x))
;; => (())


(defn eqo [x y]
  (== x y))

(run* [q]
  (eqo 'pear 'plum)
  (== true q))
;; => ()

(run* [q]
  (eqo 'plum 'plum)
  (== true q))
;; => (true)

(defn pair? [x]
  (or (lcons? x) (and (coll? x) (seq x))))

(run* [r]
  (fresh [x y]
    (== (lcons x (lcons y 'salad)) r)))
;; => ((_0 _1 . salad))

(defn pairo [p]
  (fresh [a d]
    (conso a d p)))

(run* [q]
  (pairo (lcons q q))
  (== true q))
;; => (true)

(run* [q]
  (pairo '())
  (== true q))
;; => ()

(run* [q]
  (pairo 'pair)
  (== true q))
;; => ()

(run* [x]
  (pairo x))
;; => ((_0 . _1))

(run* [r]
  (pairo (lcons r 'pear)))
;; => (_0)


