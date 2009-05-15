;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=73
;;
;; Problem 73
;; 02 July 2004
;;
;; Consider the fraction, n/d, where n and d are positive integers. If n<d and
;; HCF(n,d)=1, it is called a reduced proper fraction.
;;
;; If we list the set of reduced proper fractions for d ≤ 8 in ascending order
;; of size, we get:
;;
;; 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3,
;; 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
;;
;; It can be seen that there are 3 fractions between 1/3 and 1/2.
;;
;; How many fractions lie between 1/3 and 1/2 in the sorted set of reduced
;; proper fractions for d ≤ 10,000?
;;
;; Answer: 5066251
;;


;;
;; Accumulator.
;;
(define
  (accum      ;; Accumulate the result of iterating
    from      ;;   from 'from' (inclusive)
    to        ;;   to 'to' (inclusive)
    accum-fn  ;;   with accumulating function 'accum-fn'
    trans-fn  ;;   and transforming each iterator with 'trans-fn' function,
    init)     ;;   and initial accumulation be equal to 'init'.
  (let loop ((n from)
             (r init))
    (if (> n to)
      r
      (loop (+ n 1) (accum-fn r (trans-fn n))))))


(define (lower-bound d n)
  (let ((x (quotient d (/ 1 n))))
    (if (> (/ x d) n) x (+ x 1))))


(define (upper-bound d n)
  (let ((x (quotient d (/ 1 n))))
    (if (< (/ x d) n) x (- x 1))))


(define (f maxd)
  (accum 1
         maxd
         +
         (lambda (d)
           (accum
             (lower-bound d 1/3)
             (upper-bound d 1/2)
             +
             (lambda (n) (if (= 1 (gcd n d)) 1 0))
             0))
         0))


(define (p73)
  (f 10000))


;; end of file
;; vim: ts=4 sw=4
