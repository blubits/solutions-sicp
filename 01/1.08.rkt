#lang sicp

; Exercise 1.8
; ============
; Newton’s method for cube roots is based on the fact that if y is an
; approximation to the cube root of x, then a better approximation is given by
; the value
;
;     (x/y^2 + 2y)/3
;
; Use this formula to implement a cube-root procedure analogous to the
; square-root procedure. (In 1.3.4 we will see how to implement Newton’s
; method in general as an abstraction of these square-root and cube-root
; procedures.)

(define (square x) (* x x))

(define (curt-iter guess x)
  (if (good-enough? guess x)
      guess
      (curt-iter (improve guess x) x)))

(define (f x y)
  (/ (+ (/ x (square y))
        (* 2 y))
     3))

(define (improve guess x)
  (f x guess))

(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess))
     (* guess 0.0001)))

(define (curt x)
  (curt-iter 1.0 x))

(curt 8)
(curt 24)
(curt 0.001)
(curt 1000000000000000)
