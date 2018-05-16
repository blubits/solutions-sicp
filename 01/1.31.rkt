#lang sicp

; Exercise 1.31
; =============
; The sum procedure is only the simplest of a vast number of similar
; abstractions that can be captured as higher-order procedures. Write an
; analogous procedure called product that returns the product of the values of
; a function at points over a given range. Show how to define factorial in
; terms of product. Also use product to compute approximations to pi using the
; formula
;
;     pi   2 * 4 * 4 * 6 * 6 * 8 * ...
;     -- = ---------------------------
;      4   3 * 3 * 5 * 5 * 7 * 7 * ...

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-product n)
  (define (pi-term n)
    (/ (* 2.0 (quotient (+ n 3) 2))
       (+ 1 (* 2.0 (quotient (+ n 2) 2)))))
  (product pi-term 0 inc n))

; should be 3628800
(factorial 10)

; should be approx. pi
(* 4 (pi-product 100))

; If your product procedure generates a recursive process, write one that generates an iterative process.

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial-iter n)
  (product-iter identity 1 inc n))

(define (pi-product-iter n)
  (define (pi-term n)
    (/ (* 2.0 (quotient (+ n 3) 2))
       (+ 1 (* 2.0 (quotient (+ n 2) 2)))))
  (product-iter pi-term 0 inc n))

; should be 3628800
(factorial 10)

; should be approx. pi
(* 4 (pi-product 100))
