#lang sicp

; Exercise 2.11
; =============
; In passing, Ben also cryptically comments: "By testing the signs of the
; endpoints of the intervals, it is possible to break mul-interval into nine
; cases, only one of which requires more than two multiplications." Rewrite
; this procedure using Ben’s suggestion.

; TODO finish later LMAO i hate this problem

(define (int-pos? int) (> 0 (lower-bound int)))
(define (int-neg? int) (< 0 (upper-bound int)))
(define (int-zero? int)
  (and (>= 0 (lower-bound int)) (<= 0 (upper-bound int))))

(define (make-interval a b) (cons a b))
(define (upper-bound int) (cdr int))
(define (lower-bound int) (car int))
