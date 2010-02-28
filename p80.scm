;; 2010-02-27
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=80
;;
;; Problem 80
;; 08 October 2004
;;
;; It is well known that if the square root of a natural number is not an
;; integer, then it is irrational. The decimal expansion of such square roots
;; is infinite without any repeating pattern at all.
;;
;; The square root of two is 1.41421356237309504880..., and the digital sum of
;; the first one hundred decimal digits is 475.
;;
;; For the first one hundred natural numbers, find the total of the digital
;; sums of the first one hundred decimal digits for all the irrational square
;; roots.
;;
;; Answer: 40886


;; Turn fraction into list containing integer part of the fraction
;; (first list element) and `digits' decimal digits after decimal
;; point.
;;
;; Example:
;;   (fract->number 2/3 10) -> (0 6 6 6 6 6 6 6 6 6 6)
;;
;; Note: the list may be shorter than required if n i rational number
;; (the rest of the list would be zeros anyway):
;;   (fract->number 17/25 10) -> (0 6 8)
;;
(define (fract->number f digits)
  (let ((d (denominator f)))
    (let loop ((n (numerator f))
               (len (1+ digits))
               (res '()))
      (cond ((zero? n)    (reverse res))
            ((zero? len)  (reverse res))
            (else         (loop (* 10 (remainder n d)) 
                                (1- len) 
                                (cons (quotient n d) res)))))))


;; Approximate square root of n by k-th iteration of Newton method,
;; see http://mathworld.wolfram.com/SquareRootAlgorithms.html
;;
;; Return fraction.
;;
(define (newton-iterations n k)
  (let loop ((x-1 1)
             (k k))
    (if (zero? k)
      x-1
      (loop (* 1/2 (+ x-1 (/ n x-1))) (1- k)))))


;; List of perfect squares <= 100.
;;
(define perfect-squares-under-100
  (map (lambda (x) (* x x)) (iota 10 1)))


;; Try to solve problem 80 using k-th iteration of Newton method.
;;
;; Note: number of decimal digits is 99, NOT 100, because 
;;       (1) we need to count all the digits, NOT just the digits after decimal
;;       point, and 
;;       (2) first element of (fract->number f digits) result is less than 10,
;;       because we operate with numbers less than 100.
;;       So, the result of (fract->number f 99) will contain 100 elements
;;       exactly.
;;
(define (p80-iter k)
  ;; sum up results
  (apply + 

         ;; sum up first 100 digits
         (map (lambda (f) (apply + (fract->number f 99)))

              ;; compute approximation of square root
              (map (lambda (n) (newton-iterations n k))

                   ;; filter out perfect squares
                   (filter (lambda (n) (not (member n perfect-squares-under-100))) 

                           ;; take list of first 100 numbers
                           (iota 100 1))))))


;; Solve problem 80.
;;
(define (p80)
  (let loop ((k 8)
             (res 0))
    (let ((r (p80-iter k)))
      (format #t "~a ~a\n" k r)
      (if (= r res)
        res
        (loop (1+ k) r)))))


;; end of file
;; vim: ts=4 sw=4
