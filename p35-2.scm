;; 14 March 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=35
;;
;; Problem 35
;; 17 January 2003
;;
;; The number, 197, is called a circular prime because all rotations of the
;; digits: 197, 971, and 719, are themselves prime.
;;
;; There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37,
;; 71, 73, 79, and 97.
;;
;; How many circular primes are there below one million?
;;
;; Answer: 55
;;
;; NOTE: Sieve of Eratosthenes


(load "print.scm")


(define (number-of-digits n)
  (if (< n 10)
    1
    (+ 1 (number-of-digits (quotient n 10)))))


(define (rotate-right n multiplier)
  (+ (* (remainder n 10) multiplier)
     (quotient n 10)))


(define (p35 limit)

  (define bit-array (make-bit-string limit #t))

  (define (prime? n)
    (and (< n limit)
         (bit-string-ref bit-array n)))

  (define (circular-prime? n)

    (define (test m n rotations-left)
      (and (prime? n) 
           (or (zero? rotations-left)
               (test m 
                     (rotate-right n m) 
                     (- rotations-left 1)))))

    (let* ((digits     (number-of-digits n))
           (multiplier (expt 10 (- digits 1))))
      (test multiplier n (- digits 1))))

  (define (sieve n)

    (define (strike-out x)
      (cond ((< x limit)
             ;;(println "strike-out: " x)
             (if (bit-string-ref bit-array x)
               (bit-string-clear! bit-array x))
             (strike-out (+ x n)))))

    (cond ((< n limit)
           (if (bit-string-ref bit-array n)
             (strike-out (+ n n)))
           (sieve (+ n 1)))))

  (define (collect-circular-primes)
    (define (iter ls n)
      (if (>= n limit)
        ls
        (if (and (prime? n) (circular-prime? n))
          (iter (append ls (list n)) (+ n 1))
          (iter ls (+ n 1)))))
    (iter '() 2))

  (println "sieving...")
  (sieve 2)

  (println "collecting circular primes ...")
  (length (collect-circular-primes)))
  ;;(collect-circular-primes))


;; end of file
