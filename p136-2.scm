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
;; Answer: 2544559


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

    (dotimes (y 2 (1- limit))

      (if (zero? (remainder y 10000))
        (format #t "~a~%" y))

      (dotimes (k (1+ (quotient y 4)) 
                  (min (1- y) (quotient (+ (1- limit) (* y y)) (* 4 y))))
        (let ((n (- (* 4 k y) (* y y))))
          (hit! n))))

    (let ((res 0))
      (dotimes (n 0 (1- limit))
        (if (and (not (full? n)) (hit? n))
          (set! res (1+ res))))
      res)))


;; end of file
;; vim: sw=4 ts=4
