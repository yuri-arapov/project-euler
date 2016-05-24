;; Simple primality test


(defn prime? [n]
  (cond (= 1 n) false
        (= 2 n) true
        (zero? (rem n 2)) false
        :else (loop [d 3]
                (cond (> (* d d) n) true
                      (zero? (rem n d)) false
                      :else (recur (+ 2 d))))))


;; end of file

