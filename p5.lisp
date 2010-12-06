;; 2010-12-06
;;
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=5
;;
;; Problem 5
;; 14 December 2001
;; 
;; 2520 is the smallest number that can be divided by each of the
;; numbers from 1 to 10 without any remainder.
;; 
;; What is the smallest number that is evenly divisible by all of
;; the numbers from 1 to 20?
;;
;; Answer: 232792560
;;


;; *************************************************************
;;
;; this one-liner does the job:
;;
;; (apply lcm (range 1 20))
;;
;; note: lcm is built-in letest-common-multiplier function
;;
;; *************************************************************


(defun range (x y) (loop for i from x to y collect i))

(defun p5 () (apply #'lcm (range 1 20)))

;; end of file
