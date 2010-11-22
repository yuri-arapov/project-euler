;; 24 December 2007
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=1
;;
;; Problem 1
;; 05 October 2001
;;
;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we
;; get 3, 5, 6 and 9. The sum of these multiples is 23.
;;
;; Find the sum of all the multiples of 3 or 5 below 1000.
;;
;; Answer:
;;      233168
;; 


(defun p1 ()
  (reduce #'+
          (loop for x from 1 to 999
                when (or (zerop (rem x 3)) (zerop (rem x 5)))
                collect x)))


;; end of file
