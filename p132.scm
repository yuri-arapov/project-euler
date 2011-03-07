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


(load "miller-rabin-primality-test.scm")


;; Turn number into list of digits of given base.
;; Order of the digits preserved:
;;   (number->digits 123 10) -> (1 2 3)
(define (number->digits n base)
  (unfold-right
    zero?                   ; stop test: seed is zero
    (rcurry remainder base) ; make element from seed
    (rcurry quotient base)  ; next seed
    n))                     ; initial seed


;; Compute g power e modulo c: (g^e) % c
;; http://www.cacr.math.uwaterloo.ca/hac/about/chap14.pdf
;; HAC Algorithm 14.79
;; (found in Python source code, file longobject.c, function long_pow().
;; NOTE: each multiplication is reduced by modulo c.
(define (pow-mod g e c)
  (define (mul x y) (modulo (* x y) c))
  (fold
    (lambda (e a)
      (if (= 1 e) 
        (mul (mul a a) g)
        (mul a a)))
    1
    (number->digits e 2)))


;; Get prime that follows by given prime.
(define (next-prime p)
  (let ((n (+ 2 p)))
    (if (prime? n) n
      (next-prime n))))


;; Make list of max-len length.
;; Elements are made by successful calls of (make-fn seed),
;; next seed is obtained by (next-fn seed) call.
;; Initial seed is init.
(define (construct-list-length max-len make-fn init next-fn)
  (let loop ((len 0) (seed init) (res '()))
    (if (= len max-len)
      (reverse res)
      (let ((e (make-fn seed)))
        (if e
          (loop (1+ len) (next-fn seed) (cons e res))
          (loop len (next-fn seed) res))))))


;; Solve problem 132.
(define (p132)
  (let ((ten^9 (expt 10 9)))
    (apply 
      +
      (construct-list-length
        40 ; list of 40 elements

        (lambda (p) (and (= 1 (pow-mod 10 ten^9 p)) p))
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

        next-prime))))


;; end of file
;; vim: sw=4 ts=4
