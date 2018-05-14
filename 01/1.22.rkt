#lang sicp

; Exercise 1.22
; Most Lisp implementations include a primitive called runtime that returns an
; integer that specifies the amount of time the system has been running
; (measured, for example, in microseconds). The following timed-prime-test
; procedure, when called with an integer n, prints n and checks to see if n is
; prime. If n is prime, the procedure prints three asterisks followed by the
; amount of time used in performing the test.

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
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; Slightly modified to produce cleaner output

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

; Using this procedure, write a procedure search-for-primes that checks the
; primality of consecutive odd integers in a specified range. Use your
; procedure to find the three smallest primes larger than 1000; larger than
; 10,000; larger than 100,000; larger than 1,000,000. Note the time needed to
; test each prime. Since the testing algorithm has order of growth of Θ(n), you
; should expect that testing for primes around 10,000 should take about 10
; times as long as testing for primes around 1000. Do your timing data bear
; this out? How well do the data for 100,000 and 1,000,000 support the Θ (n)
; prediction? Is your result compatible with the notion that programs on your
; machine run in time proportional to the number of steps required for the
; computation?

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
;           time        mag
; 10^12     14956
; 10^13     68015       4.54
; 10^14     195579      2.88
; 10^15     715909      3.66
;                       ave. 3.69 approx sqrt(10)
;
; This means that our prime? procedure is indeed O(sqrt(n))
