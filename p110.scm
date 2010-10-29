;; 2010-10-25
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=110
;;
;; Problem 110
;; 02 December 2005
;;
;; In the following equation x, y, and n are positive integers.
;;   1   1   1
;;   - + - = -
;;   x   y   n
;;
;; It can be verified that when n = 1260 there are 113 distinct solutions and
;; this is the least value of n for which the total number of distinct
;; solutions exceeds one hundred.
;;
;; What is the least value of n for which the number of distinct solutions
;; exceeds four million?
;;
;; NOTE: This problem is a much more difficult version of problem 108 and as it
;; is well beyond the limitations of a brute force approach it requires a
;; clever implementation.
;;
;; Answer:


(load "combinations.scm")
(load "range.scm")
(load "product.scm")
(load "group-by.scm")


(define (all-combinations s)
  (apply append
         (map (lambda (n) (combinations s n)) 
              (range 1 (length s)))))


(define (product-all a . z)
  (fold (lambda (s res) (product res s)) a z))



(define (make-factor divizor power) (cons divizor power))
(define (factor-divizor f)          (car f))
(define (factor-power f)            (cdr f))


(define (square-factor f)           (make-factor (factor-divizor f)
                                                 (* 2 (factor-power f))))


(define (factor->number f)          (integer-expt (factor-divizor f)
                                                  (factor-power f)))


(define (factors->number s)         (apply * (map factor->number s)))


(define (make-factors-list s)
  (map (lambda (i) (make-factor (car i) (cadr i)))
       (group-by 2 s)))


(define (factors->all-divizors s)
  (let ((divizors (map factor-divizor s))
        (powers (apply product-all 
                       (map (lambda (p) (range 1 p))
                            (map factor-power s)))))
    (map (lambda (pp)
           (factors->number (map make-factor divizors (if (list? pp) pp (list pp)))))
         powers)))


(define (number-of-ds n-as-factors)
  (let ((n (factors->number n-as-factors)))
    (fold 
      (lambda (ff res)
        (+ res
           (count
             (lambda (i) (<= i n))
             (factors->all-divizors ff))))
      1 
      (all-combinations (map square-factor n-as-factors)))))

;; end of file
;; vim: ts=4 sw=4
