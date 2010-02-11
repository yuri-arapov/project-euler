;; 2010-02-09
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; Problem 64
;; 27 February 2004
;; 
;; All square roots are periodic when written as continued fractions and can be
;; written in the form:
;;
;; FIXME: 
;;                               __
;; For example, let us consider V23:
;;
;; FIXME: 
;;
;; If we continue we would get the following expansion:
;;
;; FIXME:
;;
;; The process can be summarised as follows:
;;
;; FIXME: 
;; 
;; It can be seen that the sequence is repeating. For conciseness, we use the
;;           __
;; notation V23 = [4;(1,3,1,8)], to indicate that the block (1,3,1,8) repeats
;; indefinitely.
;; 
;; The first ten continued fraction representations of (irrational) square
;; roots are:
;;   _
;;  V2=[1;(2)],          period=1
;;   _
;;  V3=[1;(1,2)],        period=2
;;   _
;;  V5=[2;(4)],          period=1
;;   _
;;  V6=[2;(2,4)],        period=2
;;   _
;;  V7=[2;(1,1,1,4)],    period=4
;;   _
;;  V8=[2;(1,4)],        period=2
;;  __
;; V10=[3;(6)],          period=1
;;  __
;; V11=[3;(3,6)],        period=2
;;  __
;; V12=[3;(2,6)],        period=2
;;  __
;; V13=[3;(1,1,1,1,6)],  period=5
;; 
;; Exactly four continued fractions, for N <= 13, have an odd period.
;; 
;; How many continued fractions for N <= 10000 have an odd period?
;; 
;; Answer: 1322


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
    (if (<= (- b a) 1)
      a
      (let* ((m (quotient (+ a b) 2))
             (mm (* m m)))
        (if (= mm n)
          m
          (if (< mm n)
            (loop m b)
            (loop a m)))))))


;; Return list of length len that contains coffs. of continued fraction that 
;; approximate square root of n.
;;
;; See 
;;   http://en.wikipedia.org/wiki/Continued_fraction
;;   http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion
;;
;; Used for testing/learning purpose.
;;
(define (square-root-as-cf n len)
  (let ((a0 (int-square-root n)))
    (if (= (* a0 a0) n)
      (list a0)
      (let loop ((m 0)
                 (d 1)
                 (len (- len 1))
                 (res (list a0)))
        (if (zero? len)
          (reverse res)
          (let* ((a    (car res))
                 (m+1  (- (* d a) m))
                 (d+1  (/ (- n (* m+1 m+1)) d))
                 (a+1  (quotient (+ a0 m+1) d+1)))
            (format #t "~a ~a: ~a\n" m+1 d+1 a+1)
            (loop m+1 d+1 (- len 1) (cons a+1 res))))))))


;; Compute next parameters of continued fraction formulae.
;;
(define (m+1  d  a   m )  (- (* d a) m))
(define (d+1  n m+1  d )  (/ (- n (* m+1 m+1)) d))
(define (a+1 a0 m+1 d+1)  (quotient (+ a0 m+1) d+1))


;; Return length of continued fraction period that approximates
;; square root of n.
;;
(define (period n)
  (let ((m0 0)
        (d0 1)
        (a0 (int-square-root n)))

    (define (next-m-d-a m d a)
      (let* ((m1 (m+1  d  a  m))
             (d1 (d+1  n m1  d))
             (a1 (a+1 a0 m1 d1)))
        (values m1 d1 a1)))

    (if (= (* a0 a0) n)
      0 ;; n is perfect square
      (receive (m1 d1 a1) (next-m-d-a m0 d0 a0)
        (let loop ((m m1)
                   (d d1)
                   (a a1)
                   (len 0))
          (if (and (> len 0) (= m m1) (= d d1) (= a a1))
            len
            (receive (m d a) (next-m-d-a m d a)
              (loop m d a (+ len 1)))))))))


;; General-purpose accumulator.
;;
(define (accum from to term init fn)
  (if (> from to)
    init
    (accum (+ from 1) to term (fn (term from) init) fn)))


;; Solve problem 64.
;;
(define (p64)
  (accum 
    2 
    10000 
    (lambda (n) (if (odd? (period n)) 1 0)) 
    0 
    +))


;; end of file
