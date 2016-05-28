;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=48
;; 
;; Problem 48
;; 18 July 2003
;;
;; The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
;;
;; Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
;;
;; Answer: 9110846700
;;

(defn limited-expt [n pow limit]
  (loop [res n, pow (dec pow)]
    (if (zero? pow)
      res
      (recur (rem (* res n) limit) (dec pow)))))


(defn p48 []
  (let [ten-digits 10000000000
        self-pow (fn [n] (limited-expt n n ten-digits))]
    (rem (apply + (map self-pow (range 1 1001))) ten-digits)))

;; end of file
