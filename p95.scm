;; 2010-07-18
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=95
;;
;; Problem 95
;; 13 May 2005
;;
;; The proper divisors of a number are all the divisors excluding the number
;; itself. For example, the proper divisors of 28 are 1, 2, 4, 7, and 14. As
;; the sum of these divisors is equal to 28, we call it a perfect number.
;;
;; Interestingly the sum of the proper divisors of 220 is 284 and the sum of
;; the proper divisors of 284 is 220, forming a chain of two numbers. For this
;; reason, 220 and 284 are called an amicable pair.
;;
;; Perhaps less well known are longer chains. For example, starting with 12496,
;; we form a chain of five numbers:
;;
;; 12496 -> 14288 -> 15472 -> 14536 -> 14264 (-> 12496 -> ...)
;;
;; Since this chain returns to its starting point, it is called an amicable
;; chain.
;;
;; Find the smallest member of the longest amicable chain with no element
;; exceeding one million.
;;
;; Answer:


(load "permutations.scm")
(load "uniq.scm")
(load "range.scm")

(load "primes-1000000.scm")
(define primes primes-1000000)
(define prime? primes-1000000-prime?)


(define (proper-divisors n)
  (if (prime? n)
    '(1)
    (let ((nn (sqrt n)))
      (let loop ((d 2)
                 (head '(1))
                 (tail '()))
        (if (> d nn)
          (append (reverse head) tail)
          (let ((m (/ n d)))
            (if (zero? (remainder n d))
              (if (= m d)
                (loop (1+ d) (cons d head) tail)
                (loop (1+ d) (cons d head) (cons m tail)))
              (loop (1+ d) head tail))))))))


(define (proper-factors n)
  (if (prime? n)
    '()
    (let ((nn (sqrt n)))
      (let loop ((n n)
                 (p primes)
                 (res '()))
        (cond ((or (null? p) (= n 1))
               (reverse res))
              (else
                (if (zero? (remainder n (car p)))
                  (loop (quotient n (car p)) p (cons (car p) res))
                  (loop n (cdr p) res))))))))


(define (proper-divisors2 n)
  (let ((pf (proper-factors n)))
    (if (null? pf)
      '(1)
      (let* ((x (map (lambda (y) (permutations-n pf y))
                     (range 1 (1- (length pf)))))
             (y (apply append x))
             (z (map (lambda (i) (apply * i)) y))
             (w (sort z >))
             (u (uniq w)))
        (cons 1 (reverse u))))))

;; end of file
;; vim: ts=4 sw=4
