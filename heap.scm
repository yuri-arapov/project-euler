;; Heap
;;


(define (make-heap size less)

  (let ((data (make-vector size #f))
        (count 0))

    (define (valid-node? node)  (< node count))

    (define (data-size)         (vector-length data))
    (define (data-ref node)     (vector-ref data node))
    (define (data-set! node val)(vector-set! data node val))

    (define (data-swap i j)
      (for-each (lambda (idx val) (data-set! idx val))
                (list i j)
                (list (data-ref j) (data-ref i))))

    (define (parent node)       (quotient node 2))
    (define (left node)         (+ 1 (* 2 node)))
    (define (right node)        (+ 2 (* 2 node)))
    (define (last-node)         (1- count))

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
        (let* ((l-node (left node))
               (r-node (right node)))
          (cond
            ((and (valid-node? l-node)
                  (less (data-ref l-node) (data-ref node)))
             (data-swap l-node node)
             (loop l-node))
            ((and (valid-node? r-node)
                  (less (data-ref r-node) (data-ref node)))
             (data-swap r-node node)
             (loop r-node))
            (else 
              #t)))))

    (define (add val) 
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
      ((add) (add (car args)))
      ((get) (get))
      ((count) count)
      ((size)  (data-size))
      (else (error "bad op:" op))))))



;; end of file
;; vim: ts=4 sw=4
