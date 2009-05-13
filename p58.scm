;; June 24, 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=58
;;
;; Problem 58
;; 05 December 2003
;;
;; Starting with 1 and spiralling anticlockwise in the following way, a square
;; spiral with side length 7 is formed.
;;
;; 37 36 35 34 33 32 31
;; 38 17 16 15 14 13 30
;; 39 18  5  4  3 12 29
;; 40 19  6  1  2 11 28
;; 41 20  7  8  9 10 27
;; 42 21 22 23 24 25 26
;; 43 44 45 46 47 48 49
;;
;; It is interesting to note that the odd squares lie along the bottom right
;; diagonal, but what is more interesting is that 8 out of the 13 numbers lying
;; along both diagonals are prime; that is, a ratio of 8/13 62%.
;;
;; If one complete new layer is wrapped around the spiral above, a square
;; spiral with side length 9 will be formed. If this process is continued, what
;; is the side length of the square spiral for which the ratio of primes along
;; both diagonals first falls below 10%?
;;
;; Answer: ???
;;      


(load "png.scm")
(define *png* (make-primes-generator))
(define (prime? x) (*png* 'prime? x))


;;(load "sieve-primes.scm")
;;(sieve-primes 10000000)



(define (spiral-len->corners len)
  (let ((ll  (* len len))
        (l-1 (- len 1)))
    (map (lambda (x) (- ll (* x l-1))) '(1 2 3))))


(define (p58)
  (let loop ((len 3)
             (ndiags 1)
             (nprimes 0)
             (prev-ratio 100))
    (let* ((nd (+ ndiags 4))
           (np (+ nprimes (length (filter prime? (spiral-len->corners len)))))
           (ratio (/ np nd)))
      (format #t "~a len ~a, ratio ~a (~a/~a)~%" 
                  (if (> ratio prev-ratio) "+" " ")
                  len 
                  (* 1. ratio) 
                  np 
                  nd)
      (if (< ratio 1/10)
        len
        (loop (+ len 2) nd np ratio)))))



;; end of file
