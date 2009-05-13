;; 20 March 2009
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=33
;;
;; Problem 33
;; 20 December 2002
;; 
;; The fraction 49/98 is a curious fraction, as an inexperienced
;; mathematician in attempting to simplify it may incorrectly believe that
;; 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
;; 
;; We shall consider fractions like, 30/50 = 3/5, to be trivial
;; examples.
;; 
;; There are exactly four non-trivial examples of this type of fraction, less
;; than one in value, and containing two digits in the numerator and
;; denominator.
;; 
;; If the product of these four fractions is given in its lowest common terms,
;; find the value of the denominator.
;; 
;; Answer: 100


(define (funny-cancelling? x y)
  (let ((x1 (remainder x 10))
        (x2 (remainder (quotient x 10) 10))
        (y1 (remainder y 10))
        (y2 (remainder (quotient y 10) 10))
        (x/y (/ x y)))
    (cond ((or (zero? x1)
               (zero? x2)
               (zero? y1)
               (zero? y2))
           #f)

          ((or (and (= x1 y1) (= (/ x2 y2) x/y))
               (and (= x1 y2) (= (/ x2 y1) x/y))
               (and (= x2 y1) (= (/ x1 y2) x/y))
               (and (= x2 y2) (= (/ x1 y1) x/y)))
           x/y)

          (else
            #f))))


(define (p33)
  (define (iter num denom res)
    (cond ((> denom 99)
           res)

          ((= num denom)
           (iter 11 (+ denom 1) res))

          (else
            (let ((f (funny-cancelling? num denom)))
              (iter (+ num 1) denom (* res (or f 1)))))))

  (iter 11 12 1))


;; end of file
