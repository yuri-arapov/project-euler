;; Project Euler utils.


(defn number->digigs [n]
  (if (zero? n) '(0)
      (loop [res '(), n n]
        (if (zero? n) res
            (recur (conj res (int (rem n 10))) (quot n 10))))))

;; end of file
