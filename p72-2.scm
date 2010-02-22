;; 2010-02-21
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=72
;;
;; Problem 72
;; 18 June 2004
;;
;; Consider the fraction, n/d, where n and d are positive integers. If nd and
;; HCF(n,d)=1, it is called a reduced proper fraction.
;;
;; If we list the set of reduced proper fractions for d <= 8 in ascending order
;; of size, we get:
;;
;; 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3,
;; 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
;;
;; It can be seen that there are 21 elements in this set.
;;
;; How many elements would be contained in the set of reduced proper fractions
;; for d <= 1,000,000?
;;
;; Answer: 303963552391
;;


(load "range.scm")
(load "primes-1000000.scm")


;; Sortcut for primality test
;;
(define prime? primes-1000000-prime?)


;; Split number into factors, bypass duplicated divisors.
;;
(define (number->factors n)

  (define (divizor? n d) (zero? (remainder n d)))

  (define (reduce n d)
    (cond ((divizor? n d)   (reduce (quotient n d) d))
          (else             n)))

  (let loop ((ps    primes-1000000)
             (n     n)
             (res   '()))
    (cond ((null? ps)
           #f)

          ((= 1 n)
           (reverse res))

          ((prime? n)
           (loop ps 1 (cons n res)))

          ((= (car ps) n)
           (loop ps 1 (cons (car ps) res)))

          ((divizor? n (car ps))
           (loop (cdr ps) 
                 (reduce n (car ps))
                 (cons (car ps) res)))

          (else
            (loop (cdr ps)
                  n
                  res)))))


;; Compute Euler function.
;;
;; See 
;;   http://ru.wikipedia.org/wiki/Функция_Эйлера
;;   http://en.wikipedia.org/wiki/Euler's_totient_function
;;
(define (phi n)
  (* n
     (apply * (map (lambda (p) (- 1 (/ 1 p)))
                   (number->factors n)))))


(define (p72)
  (apply + (map phi (range 2 1000000))))


;; end of file
;; vim: sw=4 ts=4
