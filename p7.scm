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


(define (prime? n)
  (define (loop d)
     (if (> (* d d) n)
       #t
       (if (= 0 (remainder n d))
          #f
          (loop (+ d 1)))))
  (loop 2))


(define (nth-prime n)
   (define (inc x) (+ x 1))
   (define (loop i nn)
      (if (prime? i)
        (if (= nn n)
           i
           (loop (inc i) (inc nn)))
        (loop (inc i) nn)))

   (loop 2   ;; the '2' 
         1)) ;; is   1st prime


(define (p7) (nth-prime 10001))


;;  end of file
