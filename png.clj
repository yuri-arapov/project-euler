;;
;; prime numbers generator
;;


(defn make-primes-generator []
  (let [primes (atom [2 3])
        pos    (atom 0)

        png-last
          (fn [] (get @primes (dec (count @primes))))

        set-pos!
          (fn [p] (swap! pos (fn [_] p)))

        sqr
          (fn [n] (* n n))

        divider?
          (fn [n d] (zero? (rem n d)))

        prime?
          (fn [n]
            (if (< n 2) false
              (loop [p @primes]
                (cond (empty? p) :need-to-expand
                      (> (sqr (first p)) n) true
                      (divider? n (first p)) false
                      :else (recur (rest p))))))

        compute-next-prime
          (fn [] 
            (loop [n (+ 2 (png-last))]
              (if (prime? n)
                (swap! primes conj n)
                (recur (+ 2 n)))))

        expand
          (fn [n]
            (while (<= (sqr (png-last)) n) (compute-next-prime)))

        png-first
          (fn [] (get @primes (set-pos! 0)))

        png-next
          (fn [] 
            (while (<= (count @primes) (inc @pos)) (compute-next-prime))
            (get @primes (set-pos! (inc @pos))))

        png-prime?
          (fn [n]
            (let [r (prime? n)]
              (if (= r :need-to-expand)
                (do (expand n)
                    (recur n))
                r)))

        make-enumerator
          (fn []
            (let [idx (atom -1)]
              (fn []
                (swap! idx inc)
                (while (<= (count @primes) @idx) (compute-next-prime))
                (get @primes @idx))))
        ]

    (fn [operator & args]
      (cond (= operator :first) (png-first)      ;; first prime
            (= operator :next)  (png-next)       ;; next prime (from current one)
            (= operator :last)  (png-last)       ;; last computed prime
            (= operator :show)  @primes          ;; all primes computed so far
            (= operator :count) (count @primes)  ;; number of the primes computed

            (= operator :primes)(repeatedly (make-enumerator))

            (= operator :prime?)
              (cond (empty? args) nil
                    (empty? (rest args)) (png-prime? (first args))
                    :else (map png-prime? args))

            :else nil))))


;; end of file
