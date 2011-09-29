;; Square root approxiamation as continued fraction
;;
;; http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion


;; Square of the n.
(define (sqr n) (* n n))


;; Compute |   _ |
;;         | \/n |
;;         +-   -+
;; (lower bound of square root of the n).
(define (lower-bound-of-square-root n)
  (let loop ((x 1))
    (let ((y (sqr (1+ x))))
      (if (> y n)
        x
        (loop (1+ x))))))


;; Compute continued fraction of square root of s of the length len.
;; Example:
;;   (square-root-cont-fract 2 10) -> (1 2 2 2 2 2 2 2 2 2)
(define (square-root-cont-fract s len)
  (let ((a0 (lower-bound-of-square-root s)))
    (let loop ((l  (1- len))
               (m  0)
               (d  1)
               (an (list a0)))
      (if (zero? l)
        (reverse an)
        (let* ((a (car an))
               (m+1 (- (* d a) m))
               (d+1 (/ (- s (sqr m+1)) d))
               (a+1 (quotient (+ a0 m+1) d+1)))
          (loop (1- l) m+1 d+1 (cons a+1 an)))))))


;; Compute valued of continued fraction given as a list of 
;; coeffs an.
;; Example: (expand-cond-fract '(1 2 2 2 2 2 2 2 2 2)) -> 3363/2378
(define (expand-cond-fract an)
  (define (single? x) (and (list x) (null? (cdr x))))
  (if (single? an)
    (car an)
    (+ (car an) (/ 1 (expand-cond-fract (cdr an))))))


;; end of file
;; vim: ts=4 sw=4 et
