#lang sicp

; Exercise 1.32
; Show that sum and product (Exercise 1.31) are both special cases of a still
; more general notion called accumulate that combines a collection of terms,
; using some general accumulation function:

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

; should output 55
(sum-integers 1 10)

; should output approx pi
(* 8 (pi-sum 1 1000))

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

; If your accumulate procedure generates a recursive process, write one that
; generates an iterative process. If it generates an iterative process, write
; one that generates a recursive process.

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))

(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))

(define (sum-integers-iter a b)
  (sum-iter identity a inc b))

(define (pi-sum-iter a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum-iter pi-term a pi-next b))

; should output 55
(sum-integers-iter 1 10)

; should output approx pi
(* 8 (pi-sum-iter 1 1000))

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
