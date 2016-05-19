;; Simple primality test


(defn prime? [n]
  (if (zero? (rem n 2)) false
    (loop [d 3]
      (cond (> (* d d) n) true
            (zero? (rem n d)) false
            :else (recur (+ 2 d))))))


;; end of file

