;; 2010-11-22
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; Problem 3
;; 
;; http://projecteuler.net/index.php?section=problems&id=3
;; 
;; 
;; The prime factors of 13195 are 5, 7, 13 and 29.
;; 
;; What is the largest prime factor of the number 600851475143?
;; 
;; Answer: 
;; 
;; See also ecm.java, found here (VERY fast):
;;   http://www.alpertron.com.ar/ECM.HTM
;; 


(defun p3 ()

  (defun divisorp (d n) (zerop (rem n d)))

  (defun iter (divisor n)
    (if (= 1 n)
      divisor
      (if (divisorp divisor n)
        (iter divisor (/ n divisor))
        (iter (1+ divisor) n))))

  (iter 2 600851475143))


;; end of file
