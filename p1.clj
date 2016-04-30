;; April 29, 2016
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=1
;;
;; Problem 1
;; 05 October 2001
;;
;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we
;; get 3, 5, 6 and 9. The sum of these multiples is 23.
;;
;; Find the sum of all the multiples of 3 or 5 below 1000.
;;
;; Answer:
;;      233168
;; 


(defn p1 []
  (apply + (filter (fn [n] (or (= 0 (rem n 3)) (= 0 (rem n 5))))
                   (range 1000))))


;; end of file
