#lang sicp

; Exercise 1.23
; The smallest-divisor procedure shown at the start of this section does lots
; of needless testing: After it checks to see if the number is divisible by 2
; there is no point in checking to see if it is divisible by any larger even
; numbers. This suggests that the values used for test-divisor should not be
; 2, 3, 4, 5, 6, …, but rather 2, 3, 5, 7, 9, …. To implement this change,
; define a procedure next that returns 3 if its input is equal to 2 and
; otherwise returns its input plus 2. Modify the smallest-divisor procedure
; to use (next test-divisor) instead of (+ test-divisor 1). With
; timed-prime-test incorporating this modified version of smallest-divisor, run
; the test for each of the 12 primes found in Exercise 1.22. Since this
; modification halves the number of test steps, you should expect it to run
; about twice as fast. Is this expectation confirmed? If not, what is the
; observed ratio of the speeds of the two algorithms, and how do you explain
; the fact that it is different from 2?

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (square n) (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; prime-test sequence slightly modified to produce cleaner output

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime)
                       start-time))))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))

; ---------------------------------------------------------------------

(define (even? x) (= (remainder x 2) 0))

(define (search-for-primes start end)
  (define (search-iter n)
    (cond ((<= n end) (timed-prime-test n) (search-iter (+ n 2)))))
  (search-iter (if (even? start) (inc start) start)))

; This will run ridiculously fast on modern hardware
; (current laptop is a 7th gen i7), so let's try bigger numbers
; (search-for-primes 1000 1050)
; (search-for-primes 10000 10050)
; (search-for-primes 100000 100050)
; (search-for-primes 1000000 1000050)

; We'll be getting ave(first 3) of the following:
(search-for-primes 1000000000000 1000000000063)
(search-for-primes 10000000000000 10000000000099)
(search-for-primes 100000000000000 100000000000097)
(search-for-primes 1000000000000000 1000000000000159)

; Results:
;
;           1.22        1.23        v1.22           mag
; 10^12     14956       10362       1.44
; 10^13     68015       31087       2.19            3.00
; 10^14     195579      89377       2.19            2.88
; 10^15     715909      254826      2.81            2.85
;                                   ave. 2.15       ave. 2.91
;
; There is a two-fold improvement in speed.
; Although it probably depends on all sorts of things about the environment
; you're in. I think Racket optimizes the if away, which is why it
; doesn't factor into the overhead. Or maybe if we rerun the code, the
; resulting factors will be different.
