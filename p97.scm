;; 01 April 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=97
;;
;; Problem 97
;; 10 June 2005
;;
;; The first known prime found to exceed one million digits was discovered in
;; 1999, and is a Mersenne prime of the form 2^6972593−1; it contains exactly
;; 2,098,960 digits. Subsequently other Mersenne primes, of the form 2p−1,
;; have been found which contain more digits.
;;
;; However, in 2004 there was found a massive non-Mersenne prime which
;;
;; contains 2,357,207 digits: 28433×2^7830457+1.
;;
;; Find the last ten digits of this prime number.
;;
;; Answer: 8739992577
;;


(define (fast-truncated-expt base power maxdigits)
;; compute 
;;
;;   base^power
;;
;; but do not let the result contain more than
;; maxdigits digits.
;;
;; (computes 2^7830457 truncated to last 10 digits in a blink of an eye)
;;
  (let ((truncator (expt 10 maxdigits)))
    (let loop ((res 1)
               (b base) 
               (p power))
      (cond ((> res truncator)
;;             (display "res ")
;;             (display res)
;;             (newline)
             (loop (remainder res truncator) b p))

            ((> b truncator)
;;             (display "bas ")
;;             (display b)
;;             (newline)
             (loop res (remainder b truncator) p))

            ((= p 0)
             res)

            ((even? p)
             (loop res (* b b) (/ p 2)))

            (else
              (loop (* res b) b (- p 1)))))))


(define (p97)
  (remainder (+ 1 (* 28433 (fast-truncated-expt 2 7830457 10))) (expt 10 10)))


;; end of file
