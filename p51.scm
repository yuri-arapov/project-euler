;; 2009-10-03
;;
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=51
;;
;; Problem 51
;; 29 August 2003
;;
;; By replacing the 1st digit of *3, it turns out that six of the nine
;; possible values: 13, 23, 43, 53, 73, and 83, are all prime.
;;
;; By replacing the 3rd and 4th digits of 56**3 with the same digit, this
;; 5-digit number is the first example having seven primes among the ten
;; generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663,
;; 56773, and 56993. Consequently 56003, being the first member of this family,
;; is the smallest prime with this property.
;;
;; Find the smallest prime which, by replacing part of the number (not
;; necessarily adjacent digits) with the same digit, is part of an eight prime
;; value family.
;;
;; Answer:


(load "png.scm")


(define (number->digits n)
  (if (< n 10)
    (cons n '())
    (cons (remainder n 10) (number->digits (quotient n 10)))))


(define (x n ls)
  (cond ((zero? n)
         #f)
        ((null? ls)
         #f)
        ((= 1 n)
         #t)
        (else
          (let loop ((seed ls)
                     (m    1)
                     (cur  (cdr ls)))
            (cond ((null? cur)
                   #f)
                  ((= (car seed) (car cur))
                   (if (= n (+ 1 m))
                     seed
                     (loop seed (+ 1 m) (cdr cur))))
                  (else
                     (loop cur 1 (cdr cur))))))))


(define *png* (make-primes-generator))
(define (prime? n) (*png* 'prime? n))

(define (primes n)
  (prime? n)
  (*png* 'show))


(define (nth-of-digit num n digit)
  (let loop ((num num)
             (pos 0)
             (n   n)
             (res 0))
    (cond ((zero? n)
           res)
          ((zero? num)
           0)
          (else
            (if (= digit (remainder num 10))
              (loop (quotient num 10)
                    (+ pos 1)
                    (- n 1)
                    (logior res (ash 1 pos)))
              (loop (quotient num 10)
                    (+ pos 1)
                    n
                    res))))))



;; end of file
