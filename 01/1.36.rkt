#lang sicp

; Exercise 1.36
; =============
; Modify fixed-point so that it prints the sequence of approximations it
; generates, using the newline and display primitives shown in Exercise 1.22.

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (print-guess x fx)
    (display "x = ")
    (display x)
    (display ", f(x) = ")
    (display fx)
    (newline))
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (print-guess guess next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Then find a solution to x^x = 1000 by finding a fixed point of
;
;     x -> log(1000) / log(x).
;
; (Use Scheme’s primitive log procedure, which computes natural logarithms.)
; Compare the number of steps this takes with and without average damping.
; (Note that you cannot start fixed-point with a guess of 1, as this would
; cause division by log(1) = 0.)

(define (f x) (/ (log 1000) (log x)))
(define (average x y) (/ (+ x y) 2.0))
(define (f-damped x) (average x (f x)))

(fixed-point f 10.0)
(fixed-point f-damped 10.0)
