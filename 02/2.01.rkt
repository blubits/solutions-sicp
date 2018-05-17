#lang sicp

; Exercise 2.1
; ============
; Define a better version of make-rat that handles both positive and negative
; arguments. make-rat should normalize the sign so that if the rational number
; is positive, both the numerator and denominator are positive, and if the
; rational number is negative, only the numerator is negative.

(define (sign a)
  (cond ((> a 0) 1)
        ((< a 0) -1)
        (else 0)))

(define (make-rat n d)
  (let
    ((g (gcd n d))
     (norm-n (if (= (sign n) (sign d)) (abs n) (- (abs n))))
     (norm-d (abs d)))
    (cons (/ norm-n g)
          (/ norm-d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(print-rat (make-rat 1 -2))
