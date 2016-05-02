;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=12
;;
;; Problem 12
;; 08 March 2002
;;
;; The sequence of triangle numbers is generated by adding the natural numbers.
;; So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first
;; ten terms would be:
;;
;; 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
;;
;; Let us list the factors of the first seven triangle numbers:
;;
;;       1: 1
;;       3: 1,3
;;       6: 1,2,3,6
;;      10: 1,2,5,10
;;      15: 1,3,5,15
;;      21: 1,3,7,21
;;      28: 1,2,4,7,14,28
;;
;; We can see that the 7th triangle number, 28, is the first triangle number to
;; have over five divisors.
;;
;; Which is the first triangle number to have over five-hundred divisors?
;;
;; Answer: 76576500
;;

(defn divider? [d n] (zero? (rem n d)))

(def primes-so-far [2 3 5 7])

(defn add-prime [p] (alter-var-root (var primes-so-far) #(conj %1 %2) p))

(defn factorize [n]
  (loop [res [], n n, [p & p-rest :as primes] primes-so-far]
    (cond (= 1 n) res
          (empty? primes) (do (add-prime n) (conj res n))
          (divider? p n) (recur (conj res p) (/ n p) primes)
          :else (recur res n p-rest))))

(defn dividers-count [n]
  (->> n factorize frequencies (map second) (map inc) (apply *)))

(defn p12 []
  (loop [n 2, t-number 1]
    (if (> (dividers-count t-number) 500)
      t-number
      (recur (inc n) (+ t-number n)))))

;; end of file
