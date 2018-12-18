; Create a cons-stream function
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

; Auxiliary functions
; Add a new stream-cadr for our merge function
; and a new stream-cddr for our delete function
(define stream-null? null?)

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-cadr stream) (cadr stream))

(define (stream-cddr stream) (force (cdr (force (cdr stream)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      (newline)
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


; Merge
(define (merge pheap1 pheap2)
  (cond ((isHeapEmpty? pheap1) pheap2)
        ((isHeapEmpty? pheap2) pheap1)
        ((< (stream-car pheap1) (stream-car pheap2))
         (make-heap (stream-car pheap1) (cons-stream pheap2 (stream-cadr pheap1))))
        (else (make-heap (stream-car pheap2) (cons-stream pheap1 (stream-cadr pheap2))))))

; Insert
(define (insert pheap val)
  (merge pheap (make-heap val '())))


; Find min
(define (find-min pheap)
  (if (isHeapEmpty? pheap)
      (delay (display "the heap is empty"))
      (stream-car pheap)))


; Delete min
(define (delete-min pheap)
  (define (merge-pairs pheap-pair)
    (cond ((isHeapEmpty? pheap-pair) '())
          ((isHeapEmpty? (stream-cdr pheap-pair)) (stream-car pheap-pair))
          (else (merge (merge (stream-car pheap-pair) (stream-cadr pheap-pair))
                       (merge-pairs (stream-cddr pheap-pair))))))
  (merge-pairs (stream-cadr pheap)))



; create 2 pairing heaps
(define pheap1 (make-heap 1 (make-heap 3 4)))
(define pheap2 (make-heap 2 (make-heap 5 6)))

; merge the two pairing heaps and return the parent root node being the smallest element,
; in this case 1 will be the parent root node since 1 < 2.
(merge pheap1 pheap2)

; insert 1 into pheap2 and create a new heap by calling merge in the
; insert function and display the min element of the new heap created
(find-min (insert pheap2 1))
; for this one it will return 1 for the min element since 1 is < 2 since it insert 1 into pheap2

; cheak if the heap is empty
(isHeapEmpty? pheap1)

; return the top element of the heap
; this returns 2 because the top element of the heap for pheap2 which is 2.
(find-min pheap2)