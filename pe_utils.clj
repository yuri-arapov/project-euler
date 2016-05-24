;; Project Euler utils.


(defn number->digits
  ([n]
   (number->digits n 10))
  ([n base]
   (if (zero? n) '(0)
       (loop [res '(), n n]
         (if (zero? n) res
             (recur (conj res (int (rem n base))) (quot n base)))))))


(defn digits->number
  ([digits]
   (digits->number digits 10))
  ([digits base]
   (reduce (fn [res d] (+ (* res base) d)) 0 digits)))

;; end of file
