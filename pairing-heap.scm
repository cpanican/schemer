;; base file


(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define heap (cons-stream 12 (cons-stream 7 (cons-stream 4 '()))))


;; check if the heap is empty
(define (isHeapEmpty? pheap)
  (null? pheap))

(isHeapEmpty? heap)