#lang sicp

; Exercise 1.27
; =============
; Demonstrate that the Carmichael numbers listed in Footnote 47 really do fool
; the Fermat test. That is, write a procedure that takes an integer n and tests
; whether a^n is congruent to a modulo n for every a < n, and try your procedure
; on the given Carmichael numbers.

(define (square n) (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n k)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it k))

(define (fast-prime? n)
  (define (fast-prime-iter n times)
    (cond ((= times 0) true)
          ((fermat-test n times)
             (fast-prime-iter n (- times 1)))
          (else false)))
  (fast-prime-iter n (- n 1)))

(fast-prime? 561)
(fast-prime? 1105)
(fast-prime? 1729)
(fast-prime? 2465)
(fast-prime? 2821)
(fast-prime? 6601)
