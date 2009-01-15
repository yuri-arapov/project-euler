;; 25 December 2007
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; Problem 2
;; 
;; http://projecteuler.net/index.php?section=problems&id=3
;; 
;; 
;; The prime factors of 13195 are 5, 7, 13 and 29.
;; 
;; What is the largest prime factor of the number 317584931803?
;; 
;; Answer: 3919
;; 
;; Done.
;; 
;; See also ecm.java, found here (VERY fast):
;;   http://www.alpertron.com.ar/ECM.HTM
;; 


(define (max-prime-factor n)
  (define (iter d nn)
    (if (> (* d d) nn)            ;; d*d > nn, so nn is a prime
      nn                          ;;   and this is the result we return
      (if (= (remainder nn d) 0)  ;; if d divides nn evenly, then
        (iter d (/ nn d))         ;;   divide nn and go on
        (iter (+ d 1) nn))))      ;; otherwise d is not a divisor, increase it
                                  ;;   and try again
  (iter 2 n))                     ;; start the process of factorisation


(define (p3)
 (max-prime-factor 317584931803))

 
(p3)


;; end of file
