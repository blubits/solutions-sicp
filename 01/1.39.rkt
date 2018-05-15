#lang sicp

; Exercise 1.39
; A continued fraction representation of the tangent function was published in
; 1770 by the German mathematician J.H. Lambert:
;
; tan x =   x
;         -----
;         1 - x^2
;           -------
;           3 - x^2
;             -------
;             5 - ...
;
; where x is in radians. Define a procedure (tan-cf x k) that computes an
; approximation to the tangent function based on Lambert’s formula. k
; specifies the number of terms to compute, as in Exercise 1.37.

(define (cont-frac n d k)
  (define (cont-frac-recur i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (cont-frac-recur (inc i))))))
  (cont-frac-recur 1))

(define (square x) (* x x))

(define (tan-cf x k)
  (define (ntan i)
    (if (= i 1) x (- (square x))))
  (define (dtan i) (- (* 2 i) 1))
  (cont-frac ntan dtan k))

(define pi 3.1415)
(tan-cf (/ pi 4) 20)
