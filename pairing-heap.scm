; Create a cons-stream function
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

; Auxiliary functions
; Add a new stream-cadr for our merge function
; and a new stream-cddr for our delete function
(define stream-null? null?)

(define (stream-car stream)(car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-cadr stream) (cadr stream))

(define (stream-cddr stream) (force (cdr (force (cdr stream)))))

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


; Definition of Pairing heap
(define (make-heap root trees)
  (cons-stream root (cons-stream trees '())))


; Creates an empty heap
(define (empty-heap)
  '())


; Check if the heap is empty
(define (isHeapEmpty? pheap)
  (if (equal? (empty-heap) pheap)
      #t
      #f
      ))

; Insert
(define (insert pheap val)
  (merge pheap (make-heap val '())))


; Merge
(define (merge pheap1 pheap2)
  (cond ((isHeapEmpty? pheap1) pheap2)
        ((isHeapEmpty? pheap2) pheap1)
        ((< (stream-car pheap1) (stream-car pheap2))
         (make-heap (stream-car pheap1) (cons-stream pheap2 (stream-cadr pheap1))))
        (else (make-heap (stream-car pheap2) (cons-stream pheap1 (stream-cadr pheap2))))))


; Find min
(define (find-min pheap)
  (if (isHeapEmpty? pheap)
      (delay (display "the heap is empty"))
      (stream-car make-heap)))


; Delete min
(define (delete-min pheap)
  (define (merge-pairs pheap-pair)
    (cond ((isHeapEmpty? pheap-pair) '())
          ((isHeapEmpty? (stream-cdr pheap-pair)) (stream-car pheap-pair))
          (else (merge (merge (stream-car pheap-pair) (stream-cadr pheap-pair))
                       (merge-pairs (stream-cddr pheap-pair))))))
  (merge-pairs (stream-cadr pheap)))