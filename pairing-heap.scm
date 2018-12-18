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
(define heap (cons-stream 4 (cons-stream 5 (cons-stream 6 '()))))

;(display-stream heap)

(define (make-heap root trees)
  (cons-stream root (cons-stream trees '())))

; display the stream up there
(display-stream (make-heap 5 (make-heap 2 3)))

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
(define (merge pheap1 pheap2)
  (cond ((isHeapEmpty? pheap1) pheap2)
        ((isHeapEmpty2 pheap2) pheap1)
        ((< (stream-car pheap1) (stream-car pheap2))
         (make-heap (stream-car pheap1) (stream-cons pheap2 (stream-cadr pheap1))))
        (else (make-heap (stream-car pheap2) (stream-cons pheap1 (stream-cadr pheap2))))))
      


;; find-min
(define (find-min pheap)
  (if (isHeapEmpty? pheap)
      (delay (display "the heap is empty"))
      (stream-car heap)))

;; delete-min
;; (define (delete-min ))


;; merge-pairs
;; (define (merge-pairs ))
