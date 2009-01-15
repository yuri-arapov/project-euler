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
;;  Answer:
;;       25164150
;;       25164150


;; brute force version
;; s1 = 0
;; s2 = 0
;; 
;; for n in range(1, 101):
;;         s1 += n*n
;;         s2 += n
;; 
;; print s2*s2 - s1


(define (p6)
  (define (loop s1 s2 n last)
     (if (>= n last)
       (- (* s2 s2) s1)
       (loop (+ s1 (* n n)) (+ s2 n) (+ n 1) last)))
  (loop 0 0 1 101))


;;  end of file
