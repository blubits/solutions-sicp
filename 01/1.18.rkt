#lang sicp

; Exercise 1.18
; Using the results of Exercise 1.16 and Exercise 1.17, devise a procedure that
; generates an iterative process for multiplying two integers in terms of
; adding, doubling, and halving and uses a logarithmic number of steps.

(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (even? n)
  (= (remainder n 2) 0))

(define (* a b)
  (define (*-iter a b n)
    (cond ((= b 0) n)
          ((even? b) (*-iter (double a) (halve b) n))
          (else (*-iter a (dec b) (+ a n)))))
  (*-iter a b 0))

(* 5 6)
