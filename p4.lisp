;; 2010-11-24
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; Problem 4
;; http://projecteuler.net/index.php?section=problems&id=4
;; 
;; A palindromic number reads the same both ways. The largest
;; palindrome made from the product of two 2-digit numbers is
;; 9009 = 91 x 99.
;; 
;; Find the largest palindrome made from the product of two
;; 3-digit numbers.
;; 
;; Answer: 906609
;; 



(defun quotient  (x y) (truncate (/ x y)))
(defun remainder (x y) (rem x y))

(defun take (s len) (subseq s 0 len))

(defun number->digits (n)
  (do ((n n (quotient n 10))
       (res '() (cons (remainder n 10) res)))
    ((zerop n) res)))


(defun palindromep (n)
  (let* ((digits    (number->digits n))
         (half-len  (quotient (length digits) 2))
         (l         (take          digits  half-len))
         (r         (take (reverse digits) half-len)))
    (equalp l r)))


(defun p4 ()
  (labels ((iter (n1 n2 res)
               (cond ((< n1 100)
                      res)
                     ((< n2 100)
                      (iter (- n1 1) (- n1 1) res))
                     (t
                       (let ((nn (* n1 n2)))
                         (iter n1
                               (- n2 1)
                               (if (and (palindromep nn) (> nn res))
                                 nn
                                 res)))))))
    (iter 999 999 0)))


;; end of file
;; vim: ts=4 sw=4 et
