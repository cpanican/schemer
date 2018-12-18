;; create a cons-stream function
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define stream-null? null?)

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))


(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
	(proc (stream-car s))
	(stream-for-each proc (stream-cdr s)))))


; displays the stream of the pairing heap
(define (display-stream s)
  (stream-for-each display-line s))

; prints it to the console
(define (display-line x)
  (display x))


;; make heap
(define heap (cons-stream 12 (cons-stream 7 (cons-stream 4 '()))))

;(display-stream heap)

; creates an empty heap
(define (empty-heap)
  '())

;; check if the heap is empty
(define (isHeapEmpty? pheap)
  (if (equal? (empty-heap) pheap)
      #t
      #f
      ))

;; insert
;; (define (insert ))


;; merge
;; (define (merge ))


;; find-min
(define (find-min pheap)
  (if (isHeapEmpty? pheap)
      (delay (display "the heap is empty"))
      (stream-car heap)))

(find-min heap)


;; delete-min
;; (define (delete-min ))


;; merge-pairs
;; (define (merge-pairs ))
