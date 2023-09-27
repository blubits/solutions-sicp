#lang sicp

; Exercise 1.10
; =============
; The following procedure computes a mathematical function called
; Ackermann’s function.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

; What are the values of the following expressions?

(A 1 10)
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
(A 0 (A 0 (A 0 (A 0 (A 0 32)))))
(A 0 (A 0 (A 0 (A 0 64))))
(A 0 (A 0 (A 0 128)))
(A 0 (A 0 256))
(A 0 512)
1024

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
; ...
(A 1 (A 1 4))
; ...
(A 1 16)
; ...
65536

(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
(A 2 (A 1 (A 2 1)))
(A 2 (A 1 2))
; ...
(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
; ...
(A 1 (A 1 4))
(A 1 16)
65536

; Consider the following procedures, where A is the
; procedure defined above:

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

; Give concise mathematical definitions for the functions computed
; by the procedures f, g, and h for positive integer values of n.
; For example, (k n) computes 5n^2.

(define n 4)

(f n)
(A 0 n)
(* 2 n)
; Therefore,
;     f(n) = 2n

(g n)
(A 1 n)
(A 0 (A 1 (- n 1)))
(A 0 (A 0 (A 1 (- n 2))))
; ...
;         |--- n times inside ----|
; A(0 (A 0 (A 0 (A 0... (A 0 (A 1 1))))))
; A(0 (A 0 (A 0 (A 0... (A 0 2)))))
; ...
;         |--- n times inside ----|
; (* 2 (* 2 (* 2 (* 2.. (* 2 2)))))
;
; This is just (expt 2 n), so
;     g(n) = 2^n

(h n)
(A 2 n)
(A 1 (A 2 (- n 1)))
(expt 2 (A 2 (- n 1)))
(expt 2 (A 1 (A 2 (- n 2))))
(expt 2 (expt 2 (A 2 (- n 2))))
; ...
; (expt 2 (expt 2 (expt 2... (expt 2 (A 1 1)))))
; (expt 2 (expt 2 (expt 2... (expt 2 2))))
;                       |---- n-1 times ----|
;
; so
;     h(n) = 2^2^2^... (n-1 times)
; or
;     h(n) = 2 up (n-1)
