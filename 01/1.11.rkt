#lang sicp

; Exercise 1.11
; =============
; A function f is defined by the rule that
;
;     f(n) = n if n < 3,
;            f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3.
;
; Write a procedure that computes f by means of a recursive
; process. Write a procedure that computes f by means of an
; iterative process.

(define (fr n)
  (cond ((< n 3) n)
        (else (+ (fr (- n 1))
                 (* 2 (fr (- n 2)))
                 (* 3 (fr (- n 3)))))))

; Initial values for the iterative process:
;     a = f(2)
;     b = f(1)
;     c = f(0)
; Our goal for the iterative process:
;     a = f(n+2)
;     b = f(n+1)
;     c = f(n)
; Transformations at every step:
;     a = a + 2b + 3c       f(n+3) = f(n+2) + 2f(n+1) + 3f(n)
;     b = a                 f(n+2)
;     c = b                 f(n+1)
(define (fi n)
  (define (fi-iter a b c n)
    (define an (+ a
                  (* 2 b)
                  (* 3 c)))
    (if (= n 0)
        c
        (fi-iter an a b (dec n))))
  (fi-iter 2 1 0 n))

; These should both output the same number (142)
(fr 7)
(fi 7)
