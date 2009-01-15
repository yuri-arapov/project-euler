;; 01 April 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=53
;;
;; Problem 53
;; 26 September 2003
;;
;; There are exactly ten ways of selecting three from five, 12345:
;;
;; 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
;;
;; In combinatorics, we use the notation, 5C3 = 10.
;;
;; In general,
;;
;;  n         n!
;;   C  = --------- ,
;;    r   r! (n-r)!
;;
;; where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.
;;
;; It is not until n = 23, that a value exceeds one-million: 23C10 =
;; 1144066.
;;
;; How many values of  nCr, for 1 ≤ n ≤ 100, are greater than one-million?
;;
;; Answer: 4075


(define (fac n)
  (let loop ((res 1) (n n))
    (if (= n 0)
      res
      (loop (* res n) (- n 1)))))


(define (C n r)
  (/ (fac n) (* (fac r) (fac (- n r)))))


(define (p53)
;; FIXME: there may be some optimization
;; FIXME: in order not to compute C when
;; FIXME: it's clear that result won't be
;; FIXME: greater than 1000000
  (let loop ((res   0)
             (n   100)
             (r   100))
    (let ((cc (C n r)))
      (cond ((< n 2)
             res)

            ((< r 1)
             (loop res (- n 1) (- n 1)))

;;            ((< cc 1000000)
;;             (loop res (- n 1) (- n 1)))

            ((> cc 1000000)
             (loop (+ res 1) n (- r 1)))

            (else
              (loop res n (- r 1)))))))


;; the same result but using map, reduce, filter,
;; and length
;;
;; (define (run-one n) 
;;   (map (lambda (r) (C n r)) (iota n 1)))
;; 
;; (define (run-all maxn) 
;;   (map run-one (iota maxn 1)))
;; 
;; (length (filter (lambda (x) (> x 1000000)) 
;;                 (reduce append '() (run-all 100))))


;; end of file
