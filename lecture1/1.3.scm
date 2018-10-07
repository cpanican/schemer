; Exercise 1.3. Define a procedure that takes three numbers as arguments
; and returns the sum of the squares of the two larger numbers.

(define (larger-sum-of-squares x y z)
  (- (+ (square x)
        (square y)
        (square z))
     (square (min x y z))))

(define (square x) (* x x))

(larger-sum-of-squares 1 2 3)