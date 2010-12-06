;;  2010-12-06
;;
;;  Project Euler
;; 
;;  http://projecteuler.net/index.php?section=problems&id=6
;; 
;;  Problem 6
;;  30 November 2001
;; 
;;  The sum of the squares of the first ten natural numbers is,
;;  1² + 2² + ... + 10² = 385
;; 
;;  The square of the sum of the first ten natural numbers is,
;;  (1 + 2 + ... + 10)² = 55² = 3025
;; 
;;  Hence the difference between the sum of the squares of the
;;  first ten natural numbers and the square of the sum is 3025 −
;;  385 = 2640.
;; 
;;  Find the difference between the sum of the squares of the
;;  first one hundred natural numbers and the square of the sum.
;; 
;;  Answer: 25164150


(defun p6 ()
  (let* ((s  (loop for i from 1 to 100 collect i))
         (ss (apply #'+ s)))
    (- (* ss ss)
       (reduce (lambda (res n) (+ res (* n n))) s))))


;;  end of file
