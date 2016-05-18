;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=26
;;
;; Problem 26
;; 13 September 2002
;; 
;; A unit fraction contains 1 in the numerator. The decimal representation of
;; the unit fractions with denominators 2 to 10 are given:
;; 
;;     1/2  =       0.5
;;     1/3  =       0.(3)
;;     1/4  =       0.25
;;     1/5  =       0.2
;;     1/6  =       0.1(6)
;;     1/7  =       0.(142857)
;;     1/8  =       0.125
;;     1/9  =       0.(1)
;;     1/10 =       0.1
;; 
;; Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
;; seen that 1/7 has a 6-digit recurring cycle.
;; 
;; Find the value of d < 1000 for which 1/d contains the longest recurring cycle
;; in its decimal fraction part.
;;
;; Answer:
;;      983

(defn up [n] (loop [u 10] (if (> u n) u (recur (* u 10)))))

(defn rec-cycle [n]
  (loop [m {}, u (up n), idx 0]
    (let [r (rem u n)]
      (cond (zero? r) 0
            (m u) (- idx (m u))
            :else (recur (assoc m u idx) (* r 10) (inc idx))))))


(defn max-res [[_ l1 :as r1] [_ l2 :as r2]] (if (> l1 l2) r1 r2))


(defn p26
  ([to]
   (reduce (fn [res n] (max-res res [n (rec-cycle n)])) [2 0] (range 3 (inc to))))
  ([]
   (p26 1000)))

;; end of file
