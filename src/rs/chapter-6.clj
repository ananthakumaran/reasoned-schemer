(ns rs.chapter-1
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]))

(defn anyo [g]
  (conde
   [g s#]
   [s# (anyo g)]))

(def nevero (anyo u#))

(run 1 [g]
  u#
  nevero)
;; => ()

(def alwayso (anyo s#))

(run 1 [q]
  alwayso
  (== true q))
;; => (true)

(run 5 [g]
  alwayso
  (== true g))
;; => (true true true true true)


(run 5 [g]
  (== true g)
  alwayso)
;; => (true true true true true)

(defn salo [g]
  (conde
   [s# s#]
   [s# g]))

(run 1 [q]
  (salo alwayso)
  (== true q))
;; => (true)

(run 1 [q]
  (salo nevero)
  (== true q))
;; => (true)

(run 1 [q]
  (conde
   [(== false q) alwayso]
   [s# (== true q)])
  (== true q))
;; => (true)
;; clojure.core.conde == condi


(run 5 [q]
  (conde
   [(== false q) alwayso]
   [s# (anyo (== true q))])
  (== true q))
;; => (true true true true true)

(defn teacupo [x]
  (conde
   [(== 'tea x) s#]
   [(== 'cup x) s#]
   [s# u#]))

(run 5 [r]
  (conde
   [(teacupo r) s#]
   [(== false r) s#]
   [s# u#]))
;; => (false tea cup)

(run 5 [q]
  (conde
   [(== false q) alwayso]
   [(== true q) alwayso]
   [s# u#])
  (== true q))
;; => (true true true true true)

(run 5 [q]
  (conde
   [alwayso s#]
   [s# nevero])
  (== true q))
;; => (true true true true true)

(run 1 [q]
  (all
   (conde
    [(== false q) s#]
    [s# (== true q)])
   alwayso)
  (== true q))
;; => (true)

(run 5 [q]
  (all
   (conde
    [(== false q) s#]
    [s# (== true q)])
   alwayso)
  (== true q))
;; => (true true true true true)


(run 5 [q]
  (all
   (conde
    [(== true q) s#]
    [s# (== false q)])
   alwayso)
  (== true q))
;; => (true true true true true)


(run 5 [q]
  (all
   (conde
    [s# s#]
    [s# nevero])
   alwayso)
  (== true q))
;; => (true true true true true)
