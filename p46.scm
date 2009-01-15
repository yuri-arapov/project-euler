;; 03 Apr. 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=46
;;
;; Problem 46
;; 20 June 2003
;;
;; It was proposed by Christian Goldbach that every odd composite number can be
;; written as the sum of a prime and twice a square.
;;
;;  9 =  7 + 2×1^2
;; 15 =  7 + 2×2^2
;; 21 =  3 + 2×3^2
;; 25 =  7 + 2×3^2
;; 27 = 19 + 2×2^2
;; 33 = 31 + 2×1^2
;;
;; It turns out that the conjecture was false.
;;
;; What is the smallest odd composite that cannot be written as the sum of a
;; prime and twice a square?
;;
;; Answer: 5777
;;
;; FIXME: this is bruteforce code
;;


(load "png.scm")


(define *png* (make-primes-generator))



(define (goldbach-number? n)
  (let loop ((p (*png* 'first)))
    (if (>= p n)
      #f
      (let ((r (- n p)))
        (if (odd? r)
          (loop (*png* 'next))
          (let* ((m (/ r 2))
                 (i (expt m 0.5)))
            (if (integer? i)
              #t
              (loop (*png* 'next)))))))))



(define (p46) 
  (let loop ((n 9))        ;; 9 is first composite odd number
    (cond ((*png* 'prime? n) 
           (loop (+ n 2))) ;; this is a prime, try next odd number

          ((goldbach-number? n) 
           (loop (+ n 2))) ;; jump to the next odd number

          (else 
            n))))          ;; found!



;; end of file
