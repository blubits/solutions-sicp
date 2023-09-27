#lang sicp

; Exercise 2.14
; =============
; Demonstrate that Lem is right. Investigate the behavior of the system on a
; variety of arithmetic expressions. Make some intervals A and B, and use
; them in computing the expressions A/A and A/B. You will get the
; most insight by using intervals whose width is a small percentage of the
; center value. Examine the results of the computation in center-percent form.

(define (make-interval a b) (cons a b))
(define (upper-bound int) (cdr int))
(define (lower-bound int) (car int))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

(define (make-center-percent c p) (make-center-width c (* p c)))
(define (percentage int)
  (/ (width int) (center int)))
