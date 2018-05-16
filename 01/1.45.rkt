#lang sicp

; Exercise 1.45
; =============
; We saw in 1.3.3 that attempting to compute square roots by naively finding a
; fixed point of y -> x/y does not converge, and that this can be fixed by
; average damping. The same method works for finding cube roots as fixed points
; of the average-damped y -> x/y^2. Unfortunately, the process does not work
; for fourth roots - a single average damp is not enough to make a fixed-point
; search for y -> x/y^3 converge. On the other hand, if we average damp twice
; (i.e., use the average damp of the average damp of y -> x/y^3) the
; fixed-point search does converge.
;
; Do some experiments to determine how many average damps are required to
; compute nth roots as a fixed-point search based upon repeated average damping
; of y -> x/y^(n − 1). Use this to implement a simple procedure for computing
; nth roots using fixed-point, average-damp, and the repeated procedure of
; Exercise 1.43. Assume that any arithmetic operations you need are available
; as primitives.
(define tolerance 0.00001)
(define dx 0.00001)

(define (fixed-point f first-guess)
  (define (print-guess x fx)
    (display "x = ")
    (display x)
    (display ", f(x) = ")
    (display fx)
    (newline))
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (print-guess guess next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-of-transform
         g transform guess)
  (newline)
  (fixed-point (transform g) guess))

(define (average x y) (/ (+ x y) 2.0))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (expt b n)
  (define (expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (expt-iter (* b b) (/ n 2) a))
          (else (expt-iter b (- n 1) (* a b)))))
  (expt-iter b n 1))

; Experimentation below:

(define (n-damp n)
  (repeated average-damp n))

; nth root of x, damped d times
(define (nth-root-damp n x d)
  (fixed-point-of-transform
    (lambda (y) (/ x (expt y (- n 1))))
    (n-damp d)
    1.0))

; From experimentation, you'll see that the nth root needs to be damped
; log2(n) times for the procedure to terminate. Test it on the
; expressions below:
;
; (nth-root-damp 64 100 5)
; (nth-root-damp 64 100 6)
; (nth-root-damp 65 100 5)
; (nth-root-damp 65 100 6)
; (nth-root-damp 127 100 6)
; (nth-root-damp 128 100 6)
; (nth-root-damp 128 100 7)
;
; With that in mind,
(define (log2-floor x) (inexact->exact (floor (/ (log x) (log 2)))))
(define (nth-root n x) (nth-root-damp n x (log2-floor n)))

; Should output 2.00....
(nth-root 16 (expt 2 16))
