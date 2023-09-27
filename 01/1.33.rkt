#lang sicp

; Exercise 1.33
; =============
; You can obtain an even more general version of accumulate (Exercise 1.32) by
; introducing the notion of a filter on the terms to be combined. That is,
; combine only those terms derived from values in the range that satisfy a
; specified condition. The resulting filtered-accumulate abstraction takes the
; same arguments as accumulate, together with an additional predicate of one
; argument that specifies the filter. Write filtered-accumulate as a procedure.

(define (filtered-accumulate combiner null-value filter term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

; Show how to express the following using filtered-accumulate:
; 1. The sum of the squares of the prime numbers in the interval a to b
; (assuming that you have a prime? predicate already written)

(define (square n) (* n n))

(define (square-filter k n)
  (if (and (= (remainder (square k) n) 1)
           (not (or (= k 1) (= k (- n 1)))))     ; test of trivialness
      0
      (remainder (* k k) n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square-filter (expmod base (/ exp 2) m) m)
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (miller-robin-test n)
  (define (try-it a)
     (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-robin-test n)
         (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (if (= n 1) #f (fast-prime? n 20)))

(define (sum-square-primes a b)
  (filtered-accumulate + 0 prime? square a inc b))

(sum-square-primes 1 10)

; 2. The product of all the positive integers less than n that are relatively
; prime to n (i.e., all positive integers i < n such that GCD (i, n) = 1).

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-int-rel-prime n)
  (define (rel-prime? i) (= (gcd i n) 1))
  (filtered-accumulate * 1 rel-prime? identity 0 inc (- n 1)))

(product-int-rel-prime 10)
