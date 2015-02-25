;;
;; Feb. 18, 2015
;;
;; Answer: 301
;;
;; Not soleved, but found solution in the internet.
;; The key point is that not all of 2..80 number should be included into
;; candidates list but only some very limited subset of them.
;;
;; With this adjustment problem is soleved under 1 minute.


(define (sqr n) (* n n))


(define (p152-size size)
  (let* ((v (make-vector (1+ size) 0))
         (r (make-vector (1+ size) 0))
         (f (make-vector (1+ size) #f))
         (stock (lambda (i) (vector-ref r i)))
         (val (lambda (i) (vector-ref v i)))
         (fff (lambda (i) (vector-ref f i)))
         (call-count 0)
         (bingo 0))

    (for-each (lambda (n)
                (if (<= n size) (vector-set! f n #t)))
              ;; list of possible candidates
;;            '(2 3 4 5 6 7 8 9 10 12 13 14 15 18 20 21 24 28 30 35 36 39 40 42 45 52 56 60 63 64 70 72))
              '(2 3 4 5 6 7 8 9 10 12 13 14 15 18 20 21 24 28 30 35 36 39 40 42 45 52 56 60 63    70 72))
;;            '(2 3 4 5 6 7 8 9 10 12    14 15 18 20 21 24 28 30 35 36    40 42 45    56 60 63    70 72))

    (dotimes (i 2 size)
      (vector-set! v i (/ (sqr i))))

    (vector-set! r size (val size))
    (dotimes-rev (i (1- size) 2)
      (vector-set! r i (+ (vector-ref r (1+ i)) (vector-ref v i))))

    (letrec ((helper (lambda (target acc pos)
                       (set! call-count (1+ call-count))
;;;                       (if (zero? (remainder call-count 100000))
;;;                         (format #t "~:d ~a ~a ~a___\r" call-count (reverse acc) pos (* 1. target)))

                       (cond
                         ((zero? target)
                          (set! bingo (1+ bingo))
                          (format #t "\n*** ~a (~:d)\n" (reverse acc) call-count)
                          (helper (val (car acc)) (cdr acc) pos))

                         ((> pos size) #f)

                         ((not (fff pos))
                          (helper target acc (1+ pos)))

                         (else
                           (if (>= target (val pos))
                             (helper (- target (val pos))
                                     (cons pos acc)
                                     (1+ pos)))
                           (if (and (< pos size) (<= target (stock (1+ pos))))
                             (helper target
                                     acc
                                     (1+ pos))))))))

      (helper 1/2 '() 2)
;;      (helper 1/1225 '(2 3 4 5 7 12 15 20 28) 34)
;;      (helper 1/2025 '(36 35 28 20 10 9 7 6 4 3 2) 37)
      (format #t "\n\n")
      (values bingo call-count))))


(define (p152) (p152-size 80))


;; end of file
