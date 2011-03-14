;; 2011-03-13
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=136
;;
;; Problem 136
;; 29 December 2006
;;
;; The positive integers, x, y, and z, are consecutive terms of an arithmetic
;; progression. Given that n is a positive integer, the equation, x2  y2  z2 =
;; n, has exactly one solution when n = 20:
;;
;; 132  102  72 = 20
;;
;; In fact there are twenty-five values of n below one hundred for which the
;; equation has a unique solution.
;;
;; How many values of n less than fifty million have exactly one solution?
;;
;; Answer:


(define (p136-ex limit)

  (let ((hits (make-bitvector limit))
        (full (make-bitvector limit))
        (counter 0)
        (m-counter 0))

    (define (hit? n)  (bitvector-ref hits n))
    (define (full? n) (bitvector-ref full n))

    (define (hit! n)
      (if (hit? n)
        (bitvector-set! full n #t)
        (bitvector-set! hits n #t)))

    (define (k-max)
      (quotient (1+ limit) 4))

    (dotimes (k 1 (k-max))
      (let ((kk4 (* k k 4))
            (k2  (* k 2)))

        (define (hit-y! y n) 
          (set! counter (1+ counter))
          (if (< k y) (hit! n)))

        (if (zero? (remainder k 1000))
          (begin
            (format #t "~12d ~8d ~9d~%" k counter m-counter)
            (set! counter 0)
            (set! m-counter 0)))

        (dotimes (m 1 (1- (* 2 k)))
          (set! m-counter (1+ m-counter))
          (and-let* 
            ((n (- kk4 (* m m)))
             ((< n limit))
             ((not (full? n))))
            (hit-y! (+ k2 m) n)
            (hit-y! (- k2 m) n)))))

    (let loop ((n 0) (res 0))
      (if (= n limit)
        res
        (loop (1+ n) (cond-inc (and (not (full? n)) (hit? n)) res))))))


;; end of file
;; vim: sw=4 ts=4
