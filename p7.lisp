;;  Project Euler
;; 
;;  http://projecteuler.net/index.php?section=problems&id=7
;;  
;;  Problem 7
;;  28 December 2001
;; 
;;  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
;;  that the 6th prime is 13.
;; 
;;  What is the 10001st prime number?
;;
;;  Answer: 104743


(defun dividerp (n d) (zerop (rem n d)))


(defun primep (primes n)
  (loop for p in primes never (dividerp n p)))


(defun p7 ()
  (labels ((iter (index primes n)
                 (if (primep primes n)
                   (if (= index 10001)
                     n
                     (iter (+ 1 index) (cons n primes) (+ 2 n)))
                   (iter index primes (+ 2 n)))))
    (iter 3 '(2 3) 5)))


;;  end of file
