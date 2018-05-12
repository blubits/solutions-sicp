#lang sicp

; Exercise 1.4
; Observe that our model of evaluation allows for combinations whose operators
; are compound expressions. Use this observation to describe the behavior of
; the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; The function below reduces a + |b| to (op a b)
; op is either + or - depending on the sign of b
; The if function selects the operator, so

(a-plus-abs-b 1 2)
((if (> 2 0) + -) 1 2))
((if (#t) + -) 1 2))
(+ 1 2)
3

(a-plus-abs-b 1 -2)
((if (> -2 0) + -) 1 -2))
((if (#f) + -) 1 -2))
(- 1 -2)
3
