;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=71
;;
;; Problem 71
;; 04 June 2004
;;
;; Consider the fraction, n/d, where n and d are positive integers. If nd and
;; HCF(n,d)=1, it is called a reduced proper fraction.
;;
;; If we list the set of reduced proper fractions for d 8 in ascending order of
;; size, we get:
;;
;; 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3,
;; 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
;;
;; It can be seen that 2/5 is the fraction immediately to the left of 3/7.
;;
;; By listing the set of reduced proper fractions for d 1,000,000 in ascending
;; order of size, find the numerator of the fraction immediately to the left of
;; 3/7.
;;
;; Answer: 428570


(define (p71)

  (define (number->int x) (inexact->exact (round x)))

  (define (iter nmin n d res)

    (define (next x)
      (let ((dd (+ d 1)))
        (iter (number->int (* res dd)) (number->int (* 3/7 dd)) dd x)))

    (let ((x (/ n d)))
      (cond ((> d 1000000)
             res)

            ((< n nmin)
             (next res))

            (else
              (if (and (= 1 (gcd n d)) (> x res) (< x 3/7))
                (next x)
                (iter nmin (- n 1) d res))))))

  (iter 2 1 5 2/5))



;; end of file
