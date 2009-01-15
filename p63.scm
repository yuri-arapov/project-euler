;; 28 Apr. 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=63
;;
;; Problem 63
;; 13 February 2004
;;
;; The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit
;; number, 134217728=8^9, is a ninth power.
;;
;; How many n-digit positive integers exist which are also an nth power?
;;
;; Answer: ???


(define (number-of-digits n)
  (let loop ((digits 0)
             (n      n))
    (if (< n 10)
      (+ digits 1)
      (loop (+ digits 1) (quotient n 10)))))


;; upper power bound (500) is set after some manual tests.
;; see project' forum for the math that determines upper
;; bound some smart way.
;;
;; see also p63-2.
;;
(define (p63)
  (define (loop n pwr s)
    (cond ((> n 9)
           s)
          ((> pwr 500) 
           (loop (+ n 1) 1 s))
          (else
            (if (= (number-of-digits (expt n pwr)) pwr)
              (loop n (+ pwr 1) (+ s 1))
              (loop n (+ pwr 1)    s)))))
  (loop 1 1 0))



(define (p63-2)
  (inexact->exact (apply + (map (lambda (x) (floor (/ 1 (- 1 (log10 x))))) 
                                (iota 9 1)))))



;; end of file
