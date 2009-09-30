;; 2009-09-30
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=65
;; 
;; Project Euler
;;
;; Problem 65
;; 12 March 2004
;;
;; The square root of 2 can be written as an infinite continued fraction.
;;
;;
;;  ___              1
;; V 2  = 1 + ---------------
;;                     1
;;            2 + -----------
;;                       1
;;                2 + -------
;;
;;                    2 + ...
;;                                                  _
;; The infinite continued fraction can be written, V2 = [1;(2)], (2) indicates
;;                                                 __  
;; that 2 repeats ad infinitum. In a similar way, V23 = [4;(1,3,1,8)].
;;
;; It turns out that the sequence of partial values of continued fractions for
;; square roots provide the best rational approximations. Let us consider the
;;                  _
;; convergents for V2.
;;                                                      _
;; Hence the sequence of the first ten convergents for V2 are:
;; 1, 3/2, 7/5, 17/12, 41/29, 99/70, 239/169, 577/408, 1393/985, 3363/2378, ...
;;
;;
;; What is most surprising is that the important mathematical constant,
;; e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].
;;
;; The first ten terms in the sequence of convergents for e are:
;; 2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...
;;
;; The sum of digits in the numerator of the 10th convergent is 1+4+5+7=17.
;;
;; Find the sum of digits in the numerator of the 100th convergent of the
;; continued fraction for e.
;;
;; Answer: 272
;;
;; NOTE: May need to check that GDC of resultant numerator and denominator is 1.
;;
;; NOTE: Couldn't work without Scheme's support of bignums.


;; 
;; For (number->digits n).
(load "number-digits.scm")


;;
;; Return list of first n numbers that form e approximation
;; sequence: 1,2,1,  1,4,1,  1,6,1, ...,  1,2k,1 ...
;;
(define (e-seq n)
  (let loop ((ls '())
             (m 1))
    (if (> (* (- m 1) 3) n)
      (reverse (take-right ls n))
      (loop (cons 1 (cons (* m 2) (cons 1 ls))) (+ m 1)))))


;;
;; Fraction as pair of numerator and denominator.
;;
(define (make-fraction num denom)       (cons num denom))
(define (fraction-num f)                (car f))
(define (fraction-denom f)              (cdr f))
(define (reverse-fraction f)            (make-fraction (fraction-denom f)
                                                       (fraction-num f)))

;;
;; Compute sum of number and fraction, return resultant fraction.
;;
(define (number+fraction n f)
  (make-fraction (+ (fraction-num f) (* n (fraction-denom f))) 
                 (fraction-denom f)))


;;
;; Compute fraction that gives n-th approximation of the e.
;;
(define (approx-e n)
  (number+fraction 
    2 
    (fold-right (lambda (n res) (reverse-fraction (number+fraction n res)))
                (make-fraction 0 1)
                (e-seq (- n 1)))))


;;
;; Problem 65
;;
(define (p65)
  (apply + (number->digits (fraction-num (approx-e 100)))))


;; end of file
