;; 28 March 2007
;;
;; 2010-02-14 (Three years... OMG)
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=66
;;
;; Problem 66
;; 26 March 2004
;;
;; Consider quadratic Diophantine equations of the form:
;;
;; x^2 – Dy^2 = 1
;;
;; For example, when D=13, the minimal solution in x is 6492 – 13×1802 = 1.
;;
;; It can be assumed that there are no solutions in positive integers when D is
;; square.
;;
;; By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the
;; following:
;;
;; 3^2 – 2×2^2 = 1
;; 2^2 – 3×1^2 = 1
;; 9^2 – 5×4^2 = 1
;; 5^2 – 6×2^2 = 1
;; 8^2 – 7×3^2 = 1
;;
;; Hence, by considering minimal solutions in x for D ≤ 7, the largest x is
;; obtained when D=5.
;;
;; Find the value of D ≤ 1000 in minimal solutions of x for which the largest
;; value of x is obtained.
;;
;; Answer: 661
;;


(use-modules (ice-9 receive))


;; Determine some value square of which is greater than or equal to n.
;;
(define (upper-square-bound n)
  (let loop ((x 2))
    (if (<= n (* x x)) x (loop (* x 2)))))


;; Determine x so that
;;
;;   2               2
;;  x  <=  n  < (x+1)
;;
(define (int-square-root n)
  (let loop ((a 1)
             (b (upper-square-bound n)))
    (cond ((= n (* a a))
           a)
          ((= n (* b b))
           b)
          ((<= (- b a) 1)
           a)
          (else
            (let* ((m (quotient (+ a b) 2))
                   (mm (* m m)))
              (if (= mm n)
                m
                (if (< mm n)
                  (loop m b)
                  (loop a m))))))))


;; Return list coffs. of continued fraction that 
;; approximate square root of n.
;; The list will contain non-periodic elements only.
;;
;; For example, (square-root->cf 14) will return (3 1 2 1 6),
;; and the (1 2 1 6) is a periodic part of continued fraction.
;;
;; Return list of single value if n is perfect square:
;; (square-root->cf 4) -> (2)
;;
;; See 
;;   http://en.wikipedia.org/wiki/Continued_fraction
;;   http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion
;;
(define (square-root->cf n)

  ;; Compute next parameters of continued fraction formulae.
  ;;
  (define (m+1  d  a   m )  (- (* d a) m))
  (define (d+1  n m+1  d )  (/ (- n (* m+1 m+1)) d))
  (define (a+1 a0 m+1 d+1)  (quotient (+ a0 m+1) d+1))

  (let ((m0 0)
        (d0 1)
        (a0 (int-square-root n)))

    (define (next-m-d-a m d a)
      (let* ((m1 (m+1  d  a  m))
             (d1 (d+1  n m1  d))
             (a1 (a+1 a0 m1 d1)))
        (values m1 d1 a1)))

    (define (first-iter? ls)
      (null? (cdr ls)))

    (if (= (* a0 a0) n)
      (list a0) ;; n is perfect square
      (receive (m1 d1 a1)
               (next-m-d-a m0 d0 a0)
        (let iter ((m m1)
                   (d d1)
                   (a a1)
                   (res (list a0)))
          (if (and (not (first-iter? res))
                   (= m m1)
                   (= d d1))
            (reverse res)
            (receive (m+1 d+1 a+1)
                     (next-m-d-a m d a)
              (iter m+1 d+1 a+1 (cons a res)))))))))


;; Turn continued fraction (defined as a sequence of coeffs into
;; fraction.
;;
;; Example:
;;   (cf->fraction '(3 1 2 1 6)) -> 101/27
;;
(define (cf->fraction ls)
  (if (null? (cdr ls)) ;; the last element in the list
    (car ls)
    (+ (car ls) (/ 1 (cf->fraction (cdr ls))))))


;; Solve Pell equation:
;;
;;    2     2
;;   x  - Dy  = 1
;;
;; for minimal integer x.
;;
;; Return x.
;; Return 0 if no solution exists.
;;
;; See:
;;   http://mathworld.wolfram.com/PellEquation.html
;;
;; Note:
;;   The result of (square-root->cf d) will be in
;;   (a0 a1 a2 a3 .... ar as) form.
;;   So value of index r is length of the list minus 2.
;;   If r is odd then 'as' element is dropped: (drop-right cf 1).
;;   If r is even, then result is appended with (a1 ... ar) part of 
;;   the list, i.e. first and last elemenst are dropped and result is
;;   appended to initial list:
;;   (append cf (drop-right (drop cf 1) 1)).
;;
(define (pell-equation d)
  (let ((cf (square-root->cf d)))
    (if (null? (cdr cf))  ;; cf list contains the only value:
      0                   ;; d is perfect square, no solution.
      (numerator
        (cf->fraction
          (let ((r (- (length cf) 2)))
            (if (odd? r)
              (drop-right cf 1)
              (append cf (drop-right (drop cf 1) 1)))))))))


;; Solve problem 66.
;;
;; Return values of d and corresponding x.
;;
(define (p66)
  (let loop ((d 1)
             (resd 0)
             (resx 0))
    (if (> d 1000)
      (values resd resx)
      (let ((x (pell-equation d)))
        (if (> x resx)
          (loop (+ 1 d) d x)
          (loop (+ 1 d) resd resx))))))


;; end of file
