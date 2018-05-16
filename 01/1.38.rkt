#lang sicp

; Exercise 1.38
; =============
; In 1737, the Swiss mathematician Leonhard Euler published a memoir De
; Fractionibus Continuis, which included a continued fraction expansion for
; e − 2 , where e is the base of the natural logarithms. In this fraction, the
; Ni are all 1, and the Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ….
; Write a program that uses your cont-frac procedure from Exercise 1.37 to
; approximate e, based on Euler’s expansion.

(define (cont-frac n d k)
  (define (cont-frac-recur i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (cont-frac-recur (inc i))))))
  (cont-frac-recur 1))

(define (e k)
  (define (ne i) 1)
  (define (de i)
  (if (= (remainder i 3) 2)
      (* 2 (+ 1 (quotient i 3)))
      1))
  (+ 2.0 (cont-frac ne de k)))

(e 7)
