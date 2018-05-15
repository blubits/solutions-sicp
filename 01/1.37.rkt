#lang sicp

; Exercise 1.37
; An infinite continued fraction is an expression of the form
;
; f =   N1
;     ------
;     D1 + N2
;        ------
;        D2 + N3
;           ------
;           D3 + ...
;
; As an example, one can show that the infinite continued fraction expansion
; with the Ni and the Di all equal to 1 produces 1/phi, where phi is the golden
; ratio (described in 1.2.2). One way to approximate an infinite continued
; fraction is to truncate the expansion after a given number of terms. Such a
; truncation - a so-called k-term finite continued fraction - has the form
;
;       N1
;     ------
;     D1 + N2
;        ------
;        ... + Nk
;              --
;              Dk
;
; Suppose that n and d are procedures of one argument (the term index i) that
; return the Ni and Di of the terms of the continued fraction. Define a
; procedure cont-frac such that evaluating (cont-frac n d k) computes the value
; of the k-term finite continued fraction.

(define (cont-frac n d k)
  (define (cont-frac-recur i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (cont-frac-recur (inc i))))))
  (cont-frac-recur 1))

; Check your procedure by approximating 1/phi using

(define k 12)
(/ 1 (cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                k))

; for successive values of k. How large must you make k in order to get an
; approximation that is accurate to 4 decimal places?
; ---------------------------------------------------------------------
;
; As you can see above, k = 12 for phi approx 1.6180.
;
; ---------------------------------------------------------------------
;
; If your cont-frac procedure generates a recursive process, write one that
; generates an iterative process. If it generates an iterative process, write
; one that generates a recursive process.

(define (cont-frac-iter n d k)
  (define (iter i accum)
    (if (> i k)
        accum
        (iter (inc i) (/ (n i) (+ (d i) accum)))))
  (iter 1 0))

(/ 1 (cont-frac-iter (lambda (i) 1.0)
                     (lambda (i) 1.0)
                     k))
