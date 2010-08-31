;; Binary Heap.
;; Stateful style.
;;
;; 2010-08-31
;; Yuri Arapov <yuridiceshky@gmail.com>
;;
;; Source: http://en.wikipedia.org/wiki/Binary_heap


(define (bheap-add   bheap val) (bheap 'bheap-add val))
(define (bheap-get   bheap)     (bheap 'bheap-get))
(define (bheap-count bheap)     (bheap 'bheap-count))


(define (make-bheap less)

  (let ((data '#()) ;; mutable state
        (count 0))  ;;   variables

    (define (data-size)         (vector-length data))
    (define (data-ref node)     (vector-ref data node))
    (define (data-set! node val)(vector-set! data node val))

    (define (data-swap i j)
      (for-each (lambda (idx val) (data-set! idx val))
                (list i j)
                (list (data-ref j) (data-ref i))))

    (define (data-expand)
      (let ((new-data (make-vector (if (zero? count) 1 (* 2 count)))))
        (dotimes i count
          (vector-set! new-data i (data-ref i)))
        (set! data new-data)))

    (define (parent node)       (quotient (1- node) 2))
    (define (left node)         (+ 1 (* 2 node)))
    (define (right node)        (+ 2 (* 2 node)))
    (define (last-node)         (1- count))
    (define (valid-node? node)  (< node count))

    (define (heapify-up)
      (let loop ((node (last-node)))
        (and-let* (((positive? node))
                   (prnt (parent node))
                   ((less (data-ref node) (data-ref prnt))))
          (data-swap prnt node)
          (loop prnt)))
      #t)

    (define (heapify-down)
      (let loop ((node 0))
        (let ((min-node ;; the min node among node and its childer
                (fold 
                  (lambda (i min-node) 
                    (if (and (valid-node? i)
                             (less (data-ref i) (data-ref min-node)))
                      i
                      min-node))
                  node
                  (list (left node) (right node)))))
          (if (not (= node min-node))
            (begin
              (data-swap min-node node)
              (loop min-node)))))
      #t)

    (define (add val) 
      (if (= count (data-size))
        (data-expand))
      (data-set! count val)
      (set! count (1+ count))
      (heapify-up))

    (define (get)
      (if (zero? count)
        #f
        (let ((res (data-ref 0)))
          (data-swap 0 (last-node))
          (set! count (1- count))
          (heapify-down)
          res)))

  (lambda (op . args)
    (case op
      ((bheap-add)      (add (car args)))
      ((bheap-get)      (get))
      ((bheap-count)    count)
      (else (error "BINARY HEAP: BAD OPERATION:" op))))))


;; Shuffle the list randomly.
;;
(define (shuffle s)
  (let ((len (length s)))
    (let loop ((n 0) (s s))
      (if (= n len)
        s
        (let ((r (1+ (random len))))
          (loop (1+ n)
                (append (take-right s (- len r))
                        (take s r))))))))


;; Test the binary heap.
;; Example: (bheap-test 10 <)
;;
(define (bheap-test n op)
  (let ((x (make-bheap op))
        (data (shuffle (iota n))))
    (for-each (lambda (i) (bheap-add x i)) data)
    (let ((res (map (lambda (i) (bheap-get x)) (iota n))))
      (format #t "data: ~a\n" data)
      (format #t "res:  ~a\n" res)
    (sorted? res op))))


;; end of file
;; vim: ts=4 sw=4
