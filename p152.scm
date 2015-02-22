;;
;; Feb. 18, 2015


(define (sqr n) (* n n))


(define (p152-size size)
  (let* ((v (make-vector (1+ size) 0))
         (r (make-vector (1+ size) 0))
         (stock (lambda (i) (vector-ref r i)))
         (val (lambda (i) (vector-ref v i)))
         (counter 0)
         (bingo 0))

    (dotimes (i 2 size)
      (vector-set! v i (/ (sqr i))))
    (vector-set! r size (val size))
    (dotimes-rev (i (1- size) 2)
      (vector-set! r i (+ (vector-ref r (1+ i)) (vector-ref v i))))

    (letrec ((helper (lambda (target acc start)
                       (set! counter (1+ counter))
                       (if (zero? (remainder counter 100000)) 
                         (format #t "~:d ~a ~a ~a___\r" counter (reverse acc) start (* 1. target)))
                       (cond 
;;;                         ((= bingo 1) #f)
                         ((zero? target) 
                          (set! bingo (1+ bingo)) 
                          (format #t "\n*** ~a (~:d)\n" (reverse acc) counter))

                         ((> start size) #f)

                         (else
                           (if (>= target (val start))
                             (helper (- target (val start))
                                     (cons start acc)
                                     (1+ start)))
                           (if (and (< start size) (<= target (stock (1+ start))))
                             (helper target
                                     acc
                                     (1+ start))))))))

      (helper 1/2 '() 2)
;;      (helper 1/1225 '(2 3 4 5 7 12 15 20 28) 34)
;;      (helper 1/2025 '(36 35 28 20 10 9 7 6 4 3 2) 37)
      (values bingo counter))))


(define (p152-size-2 size)
  (let* ((x (make-vector (1+ size) 0))
         (y (make-vector (1+ size) 0))
         (w (apply lcm (map sqr (iota (1- size) 2))))
         (val (lambda (i) (vector-ref x i)))
         (stk (lambda (i) (vector-ref y i)))
         (call-count 0)
         (bingo 0))

    (dotimes (i 2 size)
      (vector-set! x i (/ w (sqr i))))

    (format #t "~a\n" w)
    (format #t "~a\n" x)

    (vector-set! y size (val size))
    (dotimes-rev (i (1- size) 2)
      (vector-set! y i (+ (vector-ref y (1+ i)) (val i))))

    (letrec ((helper (lambda (target acc pos)
                       (set! call-count (1+ call-count))
                       (if (zero? (remainder call-count 100000)) 
                         (format #t "~:d ~a ~a ~a___\r" call-count (reverse acc) pos (* 1. target)))

                       (cond 
;;;                         ((= bingo 1) #f)
                         ((zero? target)
                          (set! bingo (1+ bingo))
                          (format #t "\n*** ~a (~:d)\n" (reverse acc) call-count))

                         ((> pos size) #f)

                         (else
                           (if (>= target (val pos))
                             (helper (- target (val pos))
                                     (cons pos acc)
                                     (1+ pos)))
                           (if (and (< pos size) (<= target (stk (1+ pos))))
                             (helper target
                                     acc
                                     (1+ pos))))))))

      (helper (* 2 (val 2)) '() 2)
      (values bingo call-count))))



;; end of file
