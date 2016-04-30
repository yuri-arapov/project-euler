;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=5
;;
;; Problem 5
;; 14 December 2001
;; 
;; 2520 is the smallest number that can be divided by each of the
;; numbers from 1 to 10 without any remainder.
;; 
;; What is the smallest number that is evenly divisible by all of
;; the numbers from 1 to 20?
;;
;; Answer:
;;      232792560
;;      232792560
;;
;; Done


(defn factorize [n]
  (loop [res []
         div 2
         n n]
    (cond (= 1 n) res
          (zero? (rem n div)) (recur (conj res div) div (quot n div))
          :else (recur res (inc div) n))))


(defn combine-factors [f1 f2]
  (loop [res []
         f1 f1
         f2 f2]
    (cond (and (empty? f1) (empty? f2)) res
          (empty? f1) (recur (conj res (first f2)) f1 (rest f2))
          (empty? f2) (recur (conj res (first f1)) (rest f1) f2)
          (= (first f1) (first f2)) (recur (conj res (first f1)) (rest f1) (rest f2))
          (< (first f1) (first f2)) (recur (conj res (first f1)) (rest f1) f2)
          (> (first f1) (first f2)) (recur (conj res (first f2)) f1 (rest f2)))))

(defn lcm [n & more]
  (apply 
    *
    (reduce (fn [res x] (combine-factors res (factorize x))) (factorize n) more)))

(defn p5 []
  (apply lcm (range 2 21)))



;; end of file

