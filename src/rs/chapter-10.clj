(ns rs.chapter-10
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


(run* [q]
  (conda
   [u# s#]
   [s# u#])
  (== true q))
;; => ()

(run* [q]
  (conda
   [u# s#]
   [s# s#])
  (== true q))
;; => (true)


(run* [q]
  (conda
   [s# u#]
   [s# s#])
  (== true q))
;; => ()

(run* [q]
  (conda
   [s# s#]
   [s# u#])
  (== true q))
;; => (true)

(run* [x]
  (conda
   [(== 'olive x) s#]
   [(== 'oil x) s#]
   [s# u#]))
;; => (olive)

(run* [x]
  (conda
   [(== 'virgin x) u#]
   [(== 'olive x) s#]
   [(== 'oil x) s#]
   [s# u#]))
;; => ()

(run* [q]
  (fresh [x y]
    (== 'split x)
    (== 'pea y)
    (conda
     [(== x y) (== 'split x)]
     [s# s#]))
  (== true q))
;; => (true)

(run* [q]
  (fresh [x y]
    (== 'split x)
    (== 'pea y)
    (conda
     [(== 'split x) (== x y)]
     [s# s#]))
  (== true q))
;; => ()

(defn not-pastao [x]
  (conda
   [(== 'pasta x) u#]
   [s# s#]))

(run* [x]
  (conda
   [(not-pastao x) u#]
   [s# (== 'spagetti x)]))
;; => (spagetti)

(run* [x]
  (== 'spagetti x)
  (conda
   [(not-pastao x) u#]
   [s# (== 'spagetti x)]))
;; => ()

(defn anyo [g]
  (conde
   [g s#]
   [s# (anyo g)]))

(def alwayso (anyo s#))

(run* [q]
  (condu
   [alwayso s#]
   [s# u#])
  (== true q))
;; => (true)

(run* [q]
  (condu
   [alwayso s#]
   [s# u#])
  u#
  (== true q))
;; => ()

(defn teacupo [x]
  (conde
   [(== 'tea x) s#]
   [(== 'cup x) s#]
   [s# u#]))

(run* [x]
  (onceo (teacupo x)))
;; => (tea)

(def nevero (anyo u#))

(defn salo [g]
  (conde
   [s# s#]
   [s# g]))

(run 1 [q]
  (onceo (salo nevero))
  u#)
;; => ()

(run* [r]
  (conde
   [(teacupo r) s#]
   [(== false r) s#]
   [s# u#]))
;; => (false tea cup)

(run* [r]
  (conda
   [(teacupo r) s#]
   [(== false r) s#]
   [s# u#]))
;; => (tea cup)

(run* [r]
  (== false r)
  (conda
   [(teacupo r) s#]
   [(== false r) s#]
   [s# u#]))
;; => (false)

(run* [r]
  (== false r)
  (condu
   [(teacupo r) s#]
   [(== false r) s#]
   [s# u#]))
;; => (false)

(defn bumpo [n x]
  (conde
   [(== n x) s#]
   [s# (fresh [m]
         (-o n '(1) m)
         (bumpo m x))]))

(run* [x]
  (bumpo '(1 1 1) x))
;; => ((1 1 1) (0 1 1) (1 0 1) (0 0 1) (1 1) (0 1) (1) ())

(defn gen&testo [op i j k]
  (onceo
   (fresh [x y z]
     (op x y z)
     (== i x)
     (== j y)
     (== k z))))

(run* [q]
  (gen&testo +o '(0 0 1) '(1 1) '(1 1 1))
  (== true q))
;; => (true)

(defn enumerato [op r n]
  (fresh [i j k]
    (bumpo n i)
    (bumpo n j)
    (op i j k)
    (gen&testo op i j k)
    (== (list i j k) r)))

(run* [s]
  (enumerato +o s '(1 1)))
;; => (((1 1) () (1 1))
;;     ((1 1) (0 1) (1 0 1))
;;     ((1 1) (1 1) (0 1 1))
;;     ((1 1) (1) (0 0 1))
;;     (() (1 1) (1 1))
;;     ((1) (1 1) (0 0 1))
;;     ((0 1) () (0 1))
;;     ((0 1) (1 1) (1 0 1))
;;     ((0 1) (0 1) (0 0 1))
;;     ((0 1) (1) (1 1))
;;     ((1) (0 1) (1 1))
;;     ((1) () (1))
;;     ((1) (1) (0 1))
;;     (() (0 1) (0 1))
;;     (() (1) (1))
;;     (() () ()))

(run 1 [s]
  (enumerato +o s '(1 1 1)))
;; => (((1 1 1) (0 1 1) (1 0 1 1)))
