;; 2011-03-06
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=132
;;
;; Problem 132
;; 01 December 2006
;;
;; A number consisting entirely of ones is called a repunit. We shall define
;; R(k) to be a repunit of length k.
;;
;; For example, R(10) = 1111111111 = 11*41*271*9091, and the sum of these prime
;; factors is 9414.
;;
;; Find the sum of the first forty prime factors of R(109).
;;
;; Answer: 843296


(load "scheme-utils.scm")


(load "miller-rabin-primality-test.scm")


;; Get prime that follows by given prime.
(define (next-prime p)
  (let ((n (+ 2 p)))
    (if (prime? n) n
      (next-prime n))))


;; Solve problem 132.
(define (p132)
  (apply 
    +
    (construct-list-length
      40 ; list of 40 elements

      (lambda (p) (and (= 1 (mod-pow 10 1000000000 p)) p))
      ; R(n) = (10^n - 1)/9 by definition.
      ;
      ; So 9*R(n) + 1 = 10^n
      ;
      ; The fact that A % B = 1 means, that A = B*x + 1,
      ; so if 10^n % p = 1 then 10^n = p*x + 1, where p is prime.
      ; So 9*R(n) + 1 = p*x + 1 --> 9*R(n) = p*x --> p is divisor of R(n).
      ;
      ; Therefore our task is to collect first 40 primes that match this
      ; condition: 10^n % p = 1.

      7 ; first prime to test: the 2 is prime but it's not a factor of 11111...111,
        ; the 3 is prime too but 10^9 of 1s can not be divided by 3.

      next-prime)))


;; end of file
;; vim: sw=4 ts=4
