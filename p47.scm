;; 04 June 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=47
;;
;; Problem 47
;; 04 July 2003
;;
;; The first two consecutive numbers to have two distinct prime factors are:
;;
;; 14 = 2 × 7
;; 15 = 3 × 5
;;
;; The first three consecutive numbers to have three distinct prime factors
;; are:
;;
;; 644 = 2² × 7 × 23
;; 645 = 3 × 5 × 43
;; 646 = 2 × 17 × 19.
;;
;; Find the first four consecutive integers to have four distinct primes
;; factors. What is the first of these numbers?
;;
;; Answer: 134043
;;


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


(define (p47)

  (define (bingo? n)
    (apply = 4 (map (lambda (x) (length (factorize (+ n x)))) '(0 1 2 3))))

  (let loop ((n 5))
    (if (= 4 (length (factorize n)))
      (begin
        (format #t "~a~%" n)
        (cond ((bingo? (- n 3))
               (- n 3))
              ((bingo? (- n 2))
               (- n 2))
              ((bingo? (- n 1))
               (- n 1))
              ((bingo? (- n 0))
               (- n 0))
              (else
                (loop (+ n 4)))))
      (loop (+ n 4)))))


;; end of file
