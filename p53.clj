;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=53
;;
;; Problem 53
;; 26 September 2003
;;
;; There are exactly ten ways of selecting three from five, 12345:
;;
;; 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
;;
;; In combinatorics, we use the notation, 5C3 = 10.
;;
;; In general,
;;
;;  n         n!
;;   C  = --------- ,
;;    r   r! (n-r)!
;;
;; where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.
;;
;; It is not until n = 23, that a value exceeds one-million: 23C10 =
;; 1144066.
;;
;; How many values of  nCr, for 1 ≤ n ≤ 100, are greater than one-million?
;;
;; Answer: 4075


(def *fac* (apply vector (reverse (reduce (fn [res n] (cons (* n (first res)) res)) '(1N) (range 1 101)))))

(defn fac [n] (get *fac* n))

(defn C [n r] (/ (fac n) (* (fac r) (fac (- n r)))))

(defn all-C [n] (map (partial C n) (range 1 (inc n))))

(defn p53 []
  (reduce (fn [res n] (+ res (count (filter (partial < 1000000) (all-C n))))) 0 (range 1 101)))

;; end of file
