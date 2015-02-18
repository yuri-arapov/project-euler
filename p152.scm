;;
;; Feb. 18, 2015


(define *inv-squares*
  (map (lambda (n) (/ 1 (* n n))) (iota 79 2)))


(define (p152-size size)
  (let* ((v (make-vector (1+ size) 0))
         (r (make-vector (1+ size) 0))
         (range (lambda (from to) (- (vector-ref r to) (vector-ref r (1- from)))))
         (val (lambda (i) (vector-ref v i)))
         (counter 0)
         (bingo 0))

    (dotimes (i 2 size)
      (vector-set! v i (/ (* i i)))
      (vector-set! r i (+ (vector-ref r (1- i)) (vector-ref v i))))

    (letrec ((helper (lambda (target acc start)
                       (set! counter (1+ counter))
                       (if (zero? (remainder counter 10000)) (format #t "~:d ~a ~a ~a___\r" counter (reverse acc) start (* 1. target)))
                       (cond 
                         ((zero? target) 
                          (set! bingo (1+ bingo)) 
                          (format #t "\n*** ~a (~:d)\n" (reverse acc) counter))

                         ((or (negative? target) (> start size)) #f)

                         ((> target (range start size)) #f)

                         (else
                           (helper (- target (val start))
                                   (cons start acc)
                                   (1+ start))
                           (helper target
                                   acc
                                   (1+ start)))))))

      (helper 1/2 '() 2)
;;      (helper 1/1225 '(2 3 4 5 7 12 15 20 28) 34)
      (values bingo counter))))


;; end of file
