#lang sicp

; Exercise 2.3
; ============
; Implement a representation for rectangles in a plane. (Hint: You may want to
; make use of Exercise 2.2.) In terms of your constructors and selectors,
; create procedures that compute the perimeter and the area of a given
; rectangle.

; TODO finish later LMAO this is too grr

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

; length and width are functions we can pass in to area/perimeter to
; allow different representations of rect to be passed in
; This is really just to allow multiple implementations of rect
; in the same namespace, but really the best way is to just assume
; that calling length-rect and width-rect will get the correct
; answer out
(define (area-rect rect length width)
  (* (length rect) (width rect)))
(define (perimeter-rect rect length width)
  (+ (* 2 (length rect)) (* 2 (width rect))))

; Now implement a different representation for rectangles. Can you design your
; system with suitable abstraction barriers, so that the same perimeter and
; area procedures will work using either representation?
