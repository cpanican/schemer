; Here is some code which you will find useful as you read the Abelson and
; Sussman material on streams.

; Please do not worry now about understanding define-syntax and syntax-rules:
; these are powerful constructs allowing construction of user-defined
; special forms.  In 335, it is enough to have a glimpse of their
; usefulness - we use them to define special forms delay and cons-stream.


; definition of streams for Abelson and Sussman, Section 3.5
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (lambda () exp))))


(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))


(define (force delayed-object) (delayed-object))


(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())

(define stream-null? null?)


; a test

(define (test-stream n)
  (cons-stream n (test-stream (/ n 0))))


; some examples, mostly from Abelson and Sussman


; a finite stream

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
        low 
        (stream-enumerate-interval (+ low 1) high))))


; referring to the nth element of a stream

(define (stream-ref s n)
  ((if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1)))))


; an infinite stream

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 0))



; streams can be mapped

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))

; another useful procedure

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
	(proc (stream-car s))
	(stream-for-each proc (stream-cdr s)))))


; we can view streams using display-stream
(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))



; form a list of the first n elements of a stream

(define (take n s)
  (cond ((zero? n) '())
        (else (cons (stream-car s)
                    (take (- n 1) (stream-cdr s))))))


; append two streams

(define (stream-append s t)
  (cond ((stream-null? s) t)
        (else (cons-stream
               (stream-car s)
               (stream-append (stream-cdr s) t)))))


; drop the first n elements of a stream

(define (drop n s)
  (cond ((zero? n) s)
        ((stream-null? s) the-empty-stream)
        (else (drop (- n 1) (stream-cdr s)))))


; reverse a stream

(define (stream-reverse s)

  (define (aux remaining so-far)
    (cond ((stream-null? remaining) so-far)
          (else (aux
                 (stream-cdr remaining)
                 (cons-stream (stream-car remaining)
                              so-far)))))

  (aux s the-empty-stream))






; stream-filter is a useful function

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
          (cons-stream (stream-car stream)
                       (stream-filter pred
                         (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


; here are some examples of stream-filter in action

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7))) integers))



; here is a more exciting infinite stream -- the infinite stream
; of prime numbers


(define (sieve stream)
 (cons-stream
  (stream-car stream)
  (sieve (stream-filter
           (lambda (x)
              (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))



; the practical side

; we saw earlier that chaining sequence operations is a 
; powerful way to organize many programs.  

; sometimes, however, this leads to tremendous inefficiencies.  consider
; the example from Section 3.5.1 of the text

(define (prime? n)
  (define (aux k done)
    (cond ((> k (/ n 2)) done)
	  (else (aux (+ k 1) (and done (not (divisible? n k)))))))
  (aux 2 #t))


(define (filter pred sequence)
  (cond ((null? sequence) '())
	((pred (car sequence)) (cons (car sequence)
				     (filter pred (cdr sequence))))
	(else (filter pred (cdr sequence)))))

(define (enumerate-interval a b)
  (cond ((> a b) '())
	(else (cons a (enumerate-interval (+ a 1) b)))))

(define (accumulate op init seq)
  (cond ((null? seq) init)
	(else (op (car seq)
		  (accumulate op init (cdr seq))))))

(define (sum-primes a b)
  (accumulate + 
	      0
	      (filter prime? (enumerate-interval a b))))
;;;
; this version requires large intermediate storage
;
; what about the following?
;;;;

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-accumulate op init stream)
  (cond ((stream-null? stream) init)
	(else (op (stream-car stream)
		  (stream-accumulate op init (stream-cdr stream))))))

(define (stream-sum-primes a b)
  (stream-accumulate + 
		     0
	      (stream-filter prime? (stream-enumerate-interval a b))))




;solution to Exercise 3.50 - needed in what follows

(define (gen-stream-map proc . argstreams)
 (if (stream-null? (car argstreams))
     the-empty-stream
     (cons-stream
       (apply proc (map stream-car argstreams))
       (apply gen-stream-map
                 (cons proc (map stream-cdr argstreams))))))






; defining streams implicitly 


; check this out -- what is the defined stream?

(define ones (cons-stream 1 ones))

; do you see that it is an infinite stream of 1's?
;
; another example
 
(define (add-streams s1 s2) 
 (gen-stream-map + s1 s2)) 

(define another-integers 
  (cons-stream 1 (add-streams ones another-integers)))


; another example

(define fibs
 (cons-stream 0
   (cons-stream 1 
     (add-streams (stream-cdr fibs) fibs))))
