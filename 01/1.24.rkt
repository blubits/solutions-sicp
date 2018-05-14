#lang sicp

; Exercise 1.24
; Modify the timed-prime-test procedure of Exercise 1.22 to use fast-prime?
; (the Fermat method), and test each of the 12 primes you found in that
; exercise. Since the Fermat test has Θ(log ⁡n) growth, how would you expect the
; time to test primes near 1,000,000 to compare with the time needed to test
; primes near 1000? Do your data bear this out? Can you explain any discrepancy
; you find?

(define (square n) (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

; (random) doesn't work for n greater than approx. 2^32
; so let's take the lesser of 2^32 and n as a range for random
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- (min n 4294967087) 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else false)))

; prime-test sequence slightly modified to produce cleaner output

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

; Run the Fermat test 100 times
(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
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
(search-for-primes 1000000000000000 1000000000000159)
(search-for-primes 1000000000000000000 1000000000000000031)

; Results:
;
;           time        mag
; 10^15     3008
; 10^18     3341        1.11 approx log(10^18)/log(10^15) = 1.2
;
; It is indeed equal to log(n), although as with everything this depends
; on your environment and your Lisp variant.
