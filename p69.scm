;; June 24, 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=69
;;
;; Problem 69
;; 07 May 2004
;;
;; Euler's Totient function, φ(n) [sometimes called the phi function], is used
;; to determine the number of numbers less than n which are relatively prime to
;; n. For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and
;; relatively prime to nine, φ(9)=6.
;;
;; n    Relatively Prime        φ(n)    n/φ(n)
;; 2    1                       1       2
;; 3    1,2                     2       1.5
;; 4    1,3                     2       2
;; 5    1,2,3,4                 4       1.25
;; 6    1,5                     2       3
;; 7    1,2,3,4,5,6             6       1.1666...
;; 8    1,3,5,7                 4       2
;; 9    1,2,4,5,7,8             6       1.5
;; 10   1,3,7,9                 4       2.5
;;
;; It can be seen that n=6 produces a maximum n/φ(n) for n 10.
;;
;; Find the value of n 1,000,000 for which n/φ(n) is a maximum.
;;
;; Answer: 510510


(load "png.scm")
(define *png* (make-primes-generator))


(define (factorize n)

  (define (divisor? d num) (zero? (remainder num d)))

  (define (append-to-res prime pwr res)
    (if (zero? pwr)
      res
      (cons prime res)))

  (let loop ((n n)
             (prime (*png* 'first))
             (pwr 0)
             (res '()))
    (cond ((= n 1)
           (reverse (append-to-res prime pwr res)))

          ((divisor? prime n)
           (loop (/ n prime) prime (+ pwr 1) res))

          (else
            (loop n (*png* 'next) 0 (append-to-res prime pwr res))))))


(define (p69)
  (* 2 3 5 7 11 13 17)) ;; it's the largest (below 1000000)
                        ;; composite number containing
                        ;; the most numbers of primes.
                        ;;
                        ;; figured that out looking at
                        ;; long-long-long factorize-and-compute-phi loop.
                        ;;
                        ;; see http://en.wikipedia.org/wiki/Euler's_totient_function


;; end of file
