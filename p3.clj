;; April 29, 2016
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

(defn max-prime-factor [n]
  (loop [divider 2, n n]
    (cond (> (* divider divider) n) n
          (zero? (rem n divider)) (recur divider (/ n divider))
          :else (recur (inc divider) n))))

(defn p3 []
  (max-prime-factor 317584931803))

;; end of file
