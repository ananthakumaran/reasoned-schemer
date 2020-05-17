(ns rs.chapter-7
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]))

(defn bit-xoro [x y r]
  (conde
   [(== 0 x) (== 0 y) (== 0 r)]
   [(== 1 x) (== 0 y) (== 1 r)]
   [(== 0 x) (== 1 y) (== 1 r)]
   [(== 1 x) (== 1 y) (== 0 r)]))

(run* [s]
  (fresh [x y]
    (bit-xoro x y 0)
    (== (list x y) s)))
;; => ((0 0) (1 1))

(run* [s]
  (fresh [x y]
    (bit-xoro x y 1)
    (== (list x y) s)))
;; => ((1 0) (0 1))


(run* [s]
  (fresh [x y r]
    (bit-xoro x y r)
    (== (list x y r) s)))
;; => ((0 0 0) (1 0 1) (0 1 1) (1 1 0))

(defn bit-ando [x y r]
  (conde
   [(== 0 x) (== 0 y) (== 0 r)]
   [(== 1 x) (== 0 y) (== 0 r)]
   [(== 0 x) (== 1 y) (== 0 r)]
   [(== 1 x) (== 1 y) (== 1 r)]))

(run* [s]
  (fresh [x y]
    (bit-ando x y 1)
    (== (list x y) s)))
;; => ((1 1))

(defn half-addero [x y r c]
  (all
   (bit-xoro x y r)
   (bit-ando x y c)))

(run* [r]
  (half-addero 1 1 r 1))
;; => (0)

(run* [s]
  (fresh [x y r c]
    (half-addero x y r c)
    (== (list x y r c) s)))
;; => ((0 0 0 0) (1 0 1 0) (0 1 1 0) (1 1 0 1))

(defn full-addero [b x y r c]
  (fresh [w xy wz]
    (half-addero x y w xy)
    (half-addero w b r wz)
    (bit-xoro xy wz c)))

(run* [s]
  (fresh [r c]
    (full-addero 0 1 1 r c)
    (== (list r c) s)))
;; => ((0 1))

(run* [s]
  (fresh [r c]
    (full-addero 1 1 1 r c)
    (== (list r c) s)))
;; => ((1 1))

(run* [s]
  (fresh [b x y r c]
    (full-addero b x y r c)
    (== (list b x y r c) s)))
;; => ((0 0 0 0 0)
;;     (1 0 0 1 0)
;;     (0 1 0 1 0)
;;     (1 1 0 0 1)
;;     (0 0 1 1 0)
;;     (1 0 1 0 1)
;;     (0 1 1 0 1)
;;     (1 1 1 1 1))


(defn build-num [x]
  (if (= x 0)
    '()
    (cons (rem x 2) (build-num (quot x 2)))))

(defn poso [n]
  (fresh [a d]
    (== (lcons a d) n)))

(run* [q]
  (poso '(0 1 1))
  (== true q))
;; => (true)

(run* [q]
  (poso '(1))
  (== true q))
;; => (true)

(run* [q]
  (poso '())
  (== true q))
;; => ()


(run* [q]
  (poso q))
;; => ((_0 . _1))

(defn >1o [n]
  (fresh [a ad dd]
    (== (llist a ad dd) n)))

(run* [q]
  (>1o '(0 1 1))
  (== true q))
;; => (true)

(run* [q]
  (>1o '(0 1))
  (== true q))
;; => (true)

(run* [q]
  (>1o '(1))
  (== true q))
;; => ()

(run* [q]
  (>1o '())
  (== true q))
;; => ()

(run* [q]
  (>1o q))
;; => ((_0 _1 . _2))

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

(run 3 [s]
  (fresh [x y r]
    (addero 0 x y r)
    (== (list x y r) s)))
;; => ((_0 () _0) (() (_0 . _1) (_0 . _1)) ((1) (1) (0 1)))

(run 22 [s]
  (fresh [x y r]
    (addero 0 x y r)
    (== (list x y r) s)))
;; => ((_0 () _0)
;;     (() (_0 . _1) (_0 . _1))
;;     ((1) (1) (0 1))
;;     ((1) (0 _0 . _1) (1 _0 . _1))
;;     ((1) (1 1) (0 0 1))
;;     ((0 _0 . _1) (1) (1 _0 . _1))
;;     ((1) (1 0 _0 . _1) (0 1 _0 . _1))
;;     ((1) (1 1 1) (0 0 0 1))
;;     ((1 1) (1) (0 0 1))
;;     ((1) (1 1 0 _0 . _1) (0 0 1 _0 . _1))
;;     ((0 1) (0 1) (0 0 1))
;;     ((1) (1 1 1 1) (0 0 0 0 1))
;;     ((1 0 _0 . _1) (1) (0 1 _0 . _1))
;;     ((1) (1 1 1 0 _0 . _1) (0 0 0 1 _0 . _1))
;;     ((1) (1 1 1 1 1) (0 0 0 0 0 1))
;;     ((1 1 1) (1) (0 0 0 1))
;;     ((1) (1 1 1 1 0 _0 . _1) (0 0 0 0 1 _0 . _1))
;;     ((1) (1 1 1 1 1 1) (0 0 0 0 0 0 1))
;;     ((1 1 0 _0 . _1) (1) (0 0 1 _0 . _1))
;;     ((1 1) (0 1) (1 0 1))
;;     ((1) (1 1 1 1 1 0 _0 . _1) (0 0 0 0 0 1 _0 . _1))
;;     ((0 1) (0 0 _0 . _1) (0 1 _0 . _1)))

(run* [s]
  (gen-addero 1 '(0 1 1) '(1 1) s))
;; => ((0 1 0 1))

(run* [s]
  (fresh [x y]
    (addero 0 x y '(1 0 1))
    (== (list x y) s)))
;; => (((1 0 1) ())
;;     (() (1 0 1))
;;     ((1) (0 0 1))
;;     ((0 0 1) (1))
;;     ((1 1) (0 1))
;;     ((0 1) (1 1)))

(defn +o [n m k]
  (addero 0 n m k))

(run* [s]
  (fresh [x y]
    (+o x y '(1 0 1))
    (== (list x y) s)))
;; => (((1 0 1) ())
;;     (() (1 0 1))
;;     ((1) (0 0 1))
;;     ((0 0 1) (1))
;;     ((1 1) (0 1))
;;     ((0 1) (1 1)))

(defn -o [n m k]
  (addero 0 m k n))

(run* [q]
  (-o '(0 0 0 1) '(1 0 1) q))
;; => ((1 1))


(run* [q]
  (-o '(0 1 1) '(0 1 1) q))
;; => (())

(run* [q]
  (-o '(0 1 1) '(0 0 0 1) q))
;; => ()
