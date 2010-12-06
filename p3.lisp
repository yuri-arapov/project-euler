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
;; Answer: 6857
;; 
;; See also ecm.java, found here (VERY fast):
;;   http://www.alpertron.com.ar/ECM.HTM
;; 


(defun divisorp (d n) (zerop (rem n d)))
(defun next-divizor (d) (if (= 2 d) 3 (+ 2 d)))


(defun p3 ()
  (labels ((iter (divisor n)
                 (if (= 1 n)
                   divisor
                   (if (divisorp divisor n)
                     (iter divisor                (/ n divisor))
                     (iter (next-divizor divisor) n)))))
    (iter 2 600851475143)))


(defun p33 ()
  (loop for d = 2               then (if d? d (next-divizor d))
        for n = 600851475143    then (if d? (/ n d) n)
        for d? = (divisorp d n) then (divisorp d n)
        if (= 1 n) return d))


;; end of file
