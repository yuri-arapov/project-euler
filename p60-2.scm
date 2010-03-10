;; 2009-12-29
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=60
;;
;; Problem 60
;; 02 January 2004
;;
;; The primes 3, 7, 109, and 673, are quite remarkable. By taking any two
;; primes and concatenating them in any order the result will always be prime.
;; For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of
;; these four primes, 792, represents the lowest sum for a set of four primes
;; with this property.
;;
;; Find the lowest sum for a set of five primes for which any two primes
;; concatenate to produce another prime.
;;
;; Answer: 26033 (8389 6733 5701 5197 13)
;;
;; Primality test memoization may speed the process up:
;; two bitvectors of N^2 size (where N is maximum testing number
;; 1000 for len 4 and 10000 for len 5 problems), first bitvector
;; is to check that n-th number has been already tested for being
;; a prime, and second bitvector to check if n-th number is a
;; prime.
;;
;; Execution time of (p60 9999 5): 112 seconds.


(load "miller-rabin-primality-test.scm")


;; Return number of decimal digits number n is consists of
;;
(define (decimal-digits n)
  (inexact->exact (ceiling (log10 (+ n 1)))))


;; Concatenate two numbers.
;; Example:
;;   (concat-numbers 123 456) -> 123456
;;
(define (concat-numbers a b)
  (let loop ((m 10))
    (if (< b m)
      (+ (* a m) b)
      (loop (* m 10)))))



;; Solve problem 60
;; Example:
;;   (p60 999 4) -> gives four smallest primes described in
;;                  task definition
;;                  (673 109 7 3)
;;
;;  (p60 9999 5) -> gives problem solution.
;;
(define (p60 max-num len)

  (let* ((myprime?          prime?)
         (digits            (decimal-digits max-num))
         (prime-determined? (make-bitvector (expt 10 (* 2 digits)) #f))
         (is-prime?         (make-bitvector (expt 10 (* 2 digits)) #f)))

    (define (prime? n)
      (if (bitvector-ref prime-determined? n)
        (bitvector-ref is-prime? n)
        (let ((p? (myprime? n)))
          (bitvector-set! prime-determined? n #t)
          (bitvector-set! is-prime? n p?)
          p?)))

    (define (good? a b)
      (and (prime? (concat-numbers a b))
           (prime? (concat-numbers b a))))

    (define (next-prime n)
      (let loop ((p (+ n 2)))
        (if (prime? p)
          p
          (loop (+ p 2)))))

    (define (iter candidate ls len iterno)

      (if (zero? (remainder iterno 10000))
        (format #t "~a ~a ~a\n" candidate ls len))

      (cond ((zero? len)
             ls)

            ((> candidate max-num)
             (if (null? ls)
               #f
               (iter (next-prime (car ls))
                     (cdr ls)
                     (+ 1 len)
                     (+ 1 iterno))))

            (else
              (if (every (lambda (n) (good? candidate n)) ls)
                (iter (next-prime candidate)
                      (cons candidate ls)
                      (- len 1)
                      (+ 1 iterno))
                (iter (next-prime candidate)
                      ls
                      len
                      (+ 1 iterno))))))

    (iter 3 '() len 1)))


;; end of file
