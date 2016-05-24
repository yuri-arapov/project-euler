;;
;; (permutations '())        => ()
;;
;; (permutations '(1))       => ((1))
;;
;; (permutations '(1 2 3))   => ((1 2 3) (1 3 2) (2 3 1) (2 1 3) (3 1 2) (3 2 1))
;;
;; (permutations 2 '(1 2 3)) => ((1 2) (1 3) (2 3) (2 1) (3 1) (3 2))


(defn rotations [s]
  (let [len (count s)]
    (loop [res '(), n (count s), c (cycle s)]
      (if (zero? n)
        (reverse res)
        (recur (cons (take len c) res) (dec n) (rest c))))))


(defn shorter-than? [len s]
  (cond (zero? len) false
        (empty? s) true
        :else (recur (dec len) (rest s))))


(defn permutations
  ([s]
   (cond (empty? s) '()
         (empty? (rest s)) (list s)
         :else (reduce (fn [res r]
                         (concat res (map (fn [p] (cons (first r) p))
                                          (permutations (rest r)))))
                       '()
                       (rotations s))))
  ([n s]
   (cond (or (empty? s) (shorter-than? n s)) '()
         (zero? n) '(())
         (empty? (rest s)) (list s)
         :else (reduce (fn [res r]
                         (concat res (map (fn [p] (cons (first r) p))
                                          (permutations (dec n) (rest r)))))
                       '()
                       (rotations s)))))

;; end of file
