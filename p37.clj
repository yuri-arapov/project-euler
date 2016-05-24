;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=37
;;
;; Problem 37
;;
;; 14 February 2003
;;
;; The number 3797 has an interesting property. Being prime itself, it is
;; possible to continuously remove digits from left to right, and remain prime
;; at each stage: 3797, 797, 97, and 7. Similarly we can work from right to
;; left: 3797, 379, 37, and 3.
;;
;; Find the sum of the only eleven primes that are both truncatable from left
;; to right and right to left.
;;
;; NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
;;
;; Answer:
;;      748317


(load-file "prime.clj")


;; example:
;;   (magnitude 123) => 100
;;   (magnitude 12)  => 10
;;   (magnitude 999) => 100
(defn magnitude [n]
  (int (Math/pow 10 (int (Math/log10 n)))))


(defn left-trancatable-prime? [n]
  (loop [m (magnitude n), n n]
    (cond (not (prime? n)) false
          (= 1 m) true
          :else (recur (/ m 10) (rem n m)))))


(defn right-trancatable-prime? [n]
  (cond (zero? n) true
        (not (prime? n)) false
        :else (recur (quot n 10))))


(defn p37 []
  (loop [res 0 res-count 0 n 11]
    (cond (= res-count 11)
            res
          (and (right-trancatable-prime? n) (left-trancatable-prime? n))
            (do
              (println "bingo:" (inc res-count) n)
              (recur (+ res n) (inc res-count) (+ 2 n)))
          :else
            (recur res res-count (+ 2 n)))))


;; end of file
