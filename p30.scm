;; 05 March 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=30
;;
;; Problem 30
;; 08 November 2002
;;
;; Surprisingly there are only three numbers that can be written as the sum of
;; fourth powers of their digits:
;;
;;     1634 = 1^4 + 6^4 + 3^4 + 4^4
;;     8208 = 8^4 + 2^4 + 0^4 + 8^4
;;     9474 = 9^4 + 4^4 + 7^4 + 4^4
;;
;; As 1 = 1^4 is not a sum it is not included.
;;
;; The sum of these numbers is 1634 + 8208 + 9474 = 19316.
;;
;; Find the sum of all the numbers that can be written as the sum of fifth
;; powers of their digits.
;;
;; Answer:
;;      443839
;;


(define (number->digits n)
  (define (f n)
    (if (< n 10)
      (cons n '())
      (cons (remainder n 10) (f (quotient n 10)))))
  (reverse (f n)))


(define (sum-digits-powers n digit-powers)
  (apply + (map digit-powers (number->digits n))))


(define (f pwr maxn)
  (define powers-cache (map (lambda (x) (expt x pwr)) '(0 1 2 3 4 5 6 7 8 9)))
  (define (digit-powers d) (list-ref powers-cache d)) ;; d must be 0..9
  ;;
  ;; powers-cache and digit-powers are supposed to speed up computation
  ;; of (expt x pwr)
  ;;

  (define (iter n)
    (if (> n maxn)
      '()
      (if (eqv? (sum-digits-powers n digit-powers) n)
        (cons n (iter (+ n 1)))
        (iter (+ n 1)))))
  (iter 2))


(define (p30)
  (apply + (f 5 (* 6 (expt 9 5)))))


;; end of file
