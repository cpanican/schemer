; Exercise 1.4. Observe that our model of evaluation allows for combinations
; whose operators are compound expressions. Use this observation to describe
; the behavior of the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; This function takes values a and b and returns the sum of
; a and absolute value of b.