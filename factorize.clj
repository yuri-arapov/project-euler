;;
;; factorization
;;


(load-file "png.clj")

(def factor-png (make-primes-generator))


(defn factorize
  "return list of prime factors given number consists of"
  [n]
  (if (zero? n) '()
    (loop [res '(), [p & _ :as primes] (factor-png :primes), n n]
      (if (= 1 n) (reverse res)
        (cond (> (* p p) n)     (recur (cons n res) primes 1)
              (zero? (rem n p)) (recur (cons p res) primes (/ n p))
              :else             (recur res (rest primes) n))))))


;; end of file
