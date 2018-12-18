;; streams
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))


;; make heap
(define heap (cons-stream 12 (cons-stream 7 (cons-stream 4 '()))))


; creates an empty heap
(define (empty-heap)
  '())

;; check if the heap is empty
(define (isHeapEmpty? pheap)
  (if (equal? (empty-heap) pheap)
      #t
      #f
      ))

(isHeapEmpty? (cdr heap))


;; insert
;; (define (insert ))


;; merge
;; (define (merge ))


;; find-min
;;(define (find-min pheap)


;; delete-min
;; (define (delete-min ))


;; merge-pairs
;; (define (merge-pairs ))


; we can view streams using display-stream
(define stream-null? null?)

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-for-each proc s)
  (if (stream-null? s)
      '()
      (begin
	(proc (stream-car s))
	(stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(display-stream heap)