;;
;; factorization
;;


(load-file "png.clj")

(def factor-png (make-primes-generator))


(defn factorize
  "return list of prime factors given number consists of"
  [n]
  (loop [res '(), [p & _ :as primes] (factor-png :primes), n n]
    (cond (> (* p p) n)
            (cond (not (pos? n)) '()
                  (empty? res) (list n)
                  (>= n (first res)) (reverse (cons n res))
                  :else (reverse res))
          (zero? (rem n p))
            (recur (cons p res) primes (/ n p))
          :else
            (recur res (next primes) n))))


;; end of file
