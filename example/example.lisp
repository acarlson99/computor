# example file with lisp syntax

## Vars

(def a 12)
# 12

(def b 12.4)
# 12.4

(def c (+ a b))
# 24.4

(* c b)
# 302.56

(- c)
# -24.4

(^ (* (+ 12 4) 5) 2)
# 6400

## Functions

(defun (f x) (+ 2 x))
(defun (f' y) (* (f y) 3))
(f' 4)
# 12

(defun (f' y) (f (y * 2)))
(f' 4)
# 10

## Matrices

(def m1 (matrix 4 4 7))
# ┌         ┐
# │ 7 7 7 7 │
# │ 7 7 7 7 │
# │ 7 7 7 7 │
# │ 7 7 7 7 │
# └         ┘

(def m2 (matrix 4 4 '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16))))
# ┌             ┐
# │  1  2  3  4 │
# │  5  6  7  8 │
# │  9 10 11 12 │
# │ 13 14 15 16 │
# └             ┘

(+m m1 m2)
# ┌             ┐
# │  8  9 10 11 │
# │ 12 13 14 15 │
# │ 16 17 18 19 │
# │ 20 21 22 23 │
# └             ┘

## Complex

(def c1 (+ 1 2i))
# (1.0+2.0i)
(def c2 (+ 3 4i))
# (3.0+4.0i)
(+ c1 c2)
# (4.0+6.0i)
