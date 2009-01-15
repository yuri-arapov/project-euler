;; 03 Apr. 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=56
;;
;; Problem 56
;; 07 November 2003
;;
;; A googol (10^100) is a massive number: one followed by one-hundred zeros;
;; 100^100 is almost unimaginably large: one followed by two-hundred zeros.
;; Despite their size, the sum of the digits in each number is only 1.
;;
;; Considering natural numbers of the form, a^b, where a, b < 100, what is the
;; maximum digital sum?
;;
;; Answer: 972
;;
;; FIXME: bruteforce
;;


(load "number-digits.scm")


(define (p56)
  (let loop ((a 1)
             (b 1)
             (r 0))
    (cond ((= a 100)
           r)

          ((= b 100)
           (loop (+ a 1) 1 r))

          (else
            (loop a 
                  (+ b 1) 
                  (max r (apply + (number->digits (expt a b)))))))))


;; end of file
