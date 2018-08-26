#lang sicp

; Exercise 2.10
; =============
; Ben Bitdiddle, an expert systems programmer, looks over Alyssa’s shoulder and
; comments that it is not clear what it means to divide by an interval that
; spans zero. Modify Alyssa’s code to check for this condition and to signal an
; error if it occurs.

(define (make-interval a b) (cons a b))
(define (upper-bound int) (cdr int))
(define (lower-bound int) (car int))

(define (interval-spans-zero? int)
  (and (>= 0 (lower-bound int)) (>= (upper-bound int) 0)))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (interval-spans-zero? y)
    (error "div-interval: dividing by an interval that spans 0")
    (mul-interval x
                  (make-interval
                   (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y))))))

(div-interval (make-interval 4 5) (make-interval -2 0))
