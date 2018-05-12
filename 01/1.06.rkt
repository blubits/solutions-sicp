#lang sicp

; Exercise 1.6
; Alyssa P. Hacker doesn’t see why if needs to be provided as a special form.
; “Why can’t I just define it as an ordinary procedure in terms of cond?” she
; asks. Alyssa’s friend Eva Lu Ator claims this can indeed be done, and she
; defines a new version of if:

(define (new-if predicate
                then-clause
                else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; Eva demonstrates the program for Alyssa:

(new-if (= 2 3) 0 5)
5

(new-if (= 1 1) 0 5)
0

; Delighted, Alyssa uses new-if to rewrite the square-root program:

(define (square x) (* x x))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; What happens when Alyssa attempts to use this to compute square roots?
; Explain.

; ---------------------------------------------------------------------

; Using applicative order on
(sqrt 2)
; We see that it becomes equal to
(sqrt-iter 1.0 2)
(new-if (good-enough? 1.0 2) (1.0) (sqrt-iter 1.5 2)))
(new-if (good-enough? 1.0 2) (1.0) (new-if (good-enough? 1.5 2) (1.5) (sqrt-iter 1.4167 2))))
; Because Lisp uses applicative-order evaluation to run, new-if evaluates
; the predicate and the expressions simultaneously; the <else> predicate
; evaluates indefinitely without terminating.
; The (if) construct solves this by evaluating the predicate first, and
; then evaluating the appropriate expression.

; There's a counterexample in the scheme wiki (c) dft
; (http://community.schemewiki.org/?sicp-ex-1.6)
; that goes like this:

(define (try a b)
  (if (= a 0) 1 b))
(try 0 (/ 1 0))

; This is NOT a counterexample, mainly because applicative order is run on
; (try), (/ 1 0) will be evaluated before (try) will ever get to run,
; resulting in an error. Notice that the following example runs:

(define (try a)
  (if (= a 0) 1 (/ 1 0))
(try 0)
