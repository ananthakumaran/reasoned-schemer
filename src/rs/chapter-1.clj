(ns rs.chapter-1
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]))

s#
;; => #function[clojure.core.logic/succeed]

u#
;; => #function[clojure.core.logic/fail]

(run* [q]
  u#)
;; => ()

(run* [q]
  (== true q))
;; => (true)

(run* [q]
  u#
  (== true q))
;; => ()

(run* [q]
  s#
  (== true q))
;; => (true)

(run* [q]
  s#
  (== 'corn q))
;; => (corn)

(run* [q]
  u#
  (== 'corn q))
;; => ()

(run* [q]
  s#
  (== false q))
;; => (false)

(run* [x]
  (let [x false]
    (== true x)))
;; => ()

(run* [q]
  (fresh [x]
    (== true x)
    (== true q)))
;; => (true)

(run* [x]
  (let [x false]
    (fresh [x]
      (== true x))))
;; => (_0)

(run* [r]
  (fresh [x y]
    (== (cons x (cons y '())) r)))
;; => ((_0 _1))

(run* [s]
  (fresh [t u]
    (== (cons t (cons u '())) s)))
;; => ((_0 _1))

(run* [r]
  (fresh [x]
    (let [y x]
      (fresh [x]
        (== (cons y (cons x (cons y '()))) r)))))
;; => ((_0 _1 _0))

(run* [r]
  (fresh [x]
    (let [y x]
      (fresh [x]
        (== (cons x (cons y (cons x '()))) r)))))
;; => ((_0 _1 _0))

(run* [q]
  (== false q)
  (== true q))
;; => ()

(run* [q]
  (== false q)
  (== false q))
;; => (false)

(run* [q]
  (let [x q]
    (== true x)))
;; => (true)

(run* [r]
  (fresh [x]
    (== x r)))
;; => (_0)

(run* [q]
  (fresh [x]
    (== true x)
    (== x q)))
;; => (true)

(run* [q]
  (fresh [x]
    (== x q)
    (== true x)))
;; => (true)

(run* [q]
  (fresh [x]
    (== (identical? x q) q)))
;; => (false)

(run* [q]
  (let [x q]
    (fresh [q]
      (== (identical? x q) x))))
;; => (false)

(run* [x]
  (conde
   [(== 'olive x) s#]
   [(== 'oil x) s#]
   [s# u#]))
;; => (olive oil)

(run 1 [x]
  (conde
   [(== 'olive x) s#]
   [(== 'oil x) s#]
   [s# u#]))
;; => (olive)

(run* [x]
  (conde
   [(== 'virgin x) u#]
   [(== 'olive x) s#]
   [s# s#]
   [(== 'oil x) s#]
   [s# u#]))
;; => (olive _0 oil)

(run 2 [x]
  (conde
   [(== 'extra x) s#]
   [(== 'virgin x) u#]
   [(== 'olive x) s#]
   [(== 'oil x) s#]
   [s# u#]))
;; => (extra olive)

(run* [r]
  (fresh [x y]
    (== 'split x)
    (== 'pea y)
    (== (cons x (cons y '())) r)))
;; => ((split pea))

(run* [r]
  (fresh [x y]
    (conde
     [(== 'split x) (== 'pea y)]
     [(== 'navy x) (== 'bean y)]
     [s# u#])
    (== (cons x (cons y '())) r)))
;; => ((split pea) (navy bean))


(run* [r]
  (fresh [x y]
    (conde
     [(== 'split x) (== 'pea y)]
     [(== 'navy x) (== 'bean y)]
     [s# u#])
    (== (cons x (cons y (cons 'soup '()))) r)))
;; => ((split pea soup) (navy bean soup))

(defn teacupo [x]
  (conde
   [(== 'tea x) s#]
   [(== 'cup x) s#]
   [s# u#]))

(run* [x]
  (teacupo x))
;; => (tea cup)

(run* [r]
  (fresh [x y]
    (conde
     [(teacupo x) (== true y) s#]
     [(== false x) (== true y)]
     [s# u#])
    (== (cons x (cons y '())) r)))
;; => ((false true) (tea true) (cup true))

(run* [r]
  (fresh [x y z]
    (conde
     [(== y x) (fresh [x]
                 (== z x))]
     [(fresh [x]
        (== y x)) (== z x)]
     (s# u#))
    (== (cons y (cons z '())) r)))
;; => ((_0 _1) (_0 _1))

(run* [r]
  (fresh [x y z]
    (conde
     [(== y x) (fresh [x] (== z x))]
     [(fresh [x] (== y x)) (== z x)]
     [s# u#])
    (== false x)
    (== (cons y (cons z '())) r)))
;; => ((false _0) (_0 false))

(run* [q]
  (let [a (== true q)
        b (== false q)]
    b))
;; => (false)

(run* [q]
  (let [a (== true q)
        b (fresh [x]
            (== x q)
            (== false x))
        c (conde
           [(== true q) s#]
           [s# u#])]
    b))
;; => (false)

