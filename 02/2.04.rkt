#lang sicp

; Exercise 2.4
; ============
; Here is an alternative procedural representation of pairs. For this
; representation, verify that (car (cons x y)) yields x for any objects
; x and y.

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define x 10)
(define y 12)

(car (cons x y))
(car (lambda (m) (m x y)))
((lambda (m) (m x y)) (lambda (p q) p))
((lambda (p q) p) x y)
x

; What is the corresponding definition of cdr?
; (Hint: To verify that this works, make use of the substitution model of
; 1.1.5.)

; The crucial thing here is step 4 of the subsitution; the lambda inside car
; picks the element inside the cons. So, cdr z must be
(define (cdr z)
  (z (lambda (p q) q)))

(cdr (cons x y))
