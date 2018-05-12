#lang sicp

; Exercise 1.7
; The good-enough? test used in computing square roots will not be very
; effective for finding the square roots of very small numbers. Also, in real
; computers, arithmetic operations are almost always performed with limited
; precision. This makes our test inadequate for very large numbers. Explain
; these statements, with examples showing how the test fails for small and
; large numbers. An alternative strategy for implementing good-enough? is to
; watch how guess changes from one iteration to the next and to stop when the
; change is a very small fraction of the guess. Design a square-root procedure
; that uses this kind of end test. Does this work better for small and large
; numbers?

(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; First, let's test it on a regular number:
(sqrt 5)
2.2360688956433634
; which is precise up to 5 decimal places.sicp

; The key to a good good-enough? function is to adapt it based on the
; anticipated solution. The current precision that good-enough? expects,
; for example, is not good enough when sqrt(x) is near to (1-2 decimal places
; away) or less than 0.001. Relative to sqrt(x), it is too large, so it
; terminates too early. For example,
(sqrt 0.0001)
0.03230844833048122
; which is far away from 0.01.

; Meanwhile, arithmetic using big numbers is incredibly inprecise, so 0.001
; is effectively 0. Let's say we run
; (sqrt 100000000000000)
; If we try to run good-enough? on values around this number (that's what
; (average) does, basically) the number is so imprecise that you'll never
; get 0.001, and your square root function dies.

; Improving good-enough? using the suggested strategy, we get
(define (good-enough-alt? guess x)
  (< (abs (- (improve guess x) guess))
     (* guess 0.0001)))

(define (sqrt-iter-alt guess x)
  (if (good-enough-alt? guess x)
      guess
      (sqrt-iter-alt (improve guess x) x)))

(define (sqrt-alt x)
  (sqrt-iter-alt 1.0 x))

(sqrt-alt 5)
(sqrt-alt 0.0001)
(sqrt-alt 100000000000000)
; In case (1), the answer is more accurate.
; In case (2), the procedure actually terminates, and the answer is relatively
; accurate.
