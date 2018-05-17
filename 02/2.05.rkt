#lang sicp

; Exercise 2.5
; ============
; Show that we can represent pairs of nonnegative integers using only numbers
; and arithmetic operations if we represent the pair a and b as the integer
; that is the product 2^a*3^b . Give the corresponding definitions of the
; procedures cons, car, and cdr.

(define (expt b n)
  (define (expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (expt-iter (* b b) (/ n 2) a))
          (else (expt-iter b (- n 1) (* a b)))))
  (expt-iter b n 1))

(define (div3? x) (= (remainder x 3) 0))
(define (div2? x) (= (remainder x 2) 0))

(define (cons a b) (* (expt 2 a) (expt 3 b)))
(define (car z)
  (cond ((<= z 1) 0)
        ((div3? z) (car (/ z 3)))
        (else (+ 1 (car (/ z 2))))))
(define (cdr z)
  (cond ((<= z 1) 0)
        ((div2? z) (cdr (/ z 2)))
        (else (+ 1 (cdr (/ z 3))))))

(car (cons 0 1))
(cdr (cons 0 1))
