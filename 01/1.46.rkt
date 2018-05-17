#lang sicp

; Exercise 1.46
; =============
; Several of the numerical methods described in this chapter are instances of
; an extremely general computational strategy known as iterative improvement.
; Iterative improvement says that, to compute something, we start with an
; initial guess for the answer, test if the guess is good enough, and otherwise
; improve the guess and continue the process using the improved guess as the
; new guess.
;
; Write a procedure iterative-improve that takes two procedures as arguments: a
; method for telling whether a guess is good enough and a method for improving
; a guess. iterative-improve should return as its value a procedure that takes
; a guess as argument and keeps improving the guess until it is good enough.

(define (iterative-improve good-enough? improve)
  (define (try x)
    (if (good-enough? x)
        (improve x)
        (try (improve x))))
  try)

; Rewrite the sqrt procedure of 1.1.7 and the fixed-point procedure of 1.3.3 in
; terms of iterative-improve.

(define tolerance 0.0001)
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2.0))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (close-enough? x y)
  (< (abs (- x y)) tolerance))

(define (sqrt x)
  (define (good-enough-sqrt? guess)
    (close-enough? (square guess) x))
  (define (improve-sqrt guess)
    ((average-damp (lambda (y) (/ x y))) guess))
  ((iterative-improve good-enough-sqrt? improve-sqrt) 1.0))

(define (fixed-point f first-guess)
  (define (good-enough-fp? guess)
    (close-enough? guess (f guess)))
  (define (improve-fp guess)
    (f guess))
  ((iterative-improve good-enough-fp? improve-fp) first-guess))

(sqrt 5)

(define (phi x) (+ 1 (/ 1.0 x)))
(fixed-point phi 1.0)
