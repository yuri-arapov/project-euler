;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=47
;;
;; Problem 47
;; 04 July 2003
;;
;; The first two consecutive numbers to have two distinct prime factors are:
;;
;; 14 = 2 × 7
;; 15 = 3 × 5
;;
;; The first three consecutive numbers to have three distinct prime factors
;; are:
;;
;; 644 = 2² × 7 × 23
;; 645 = 3 × 5 × 43
;; 646 = 2 × 17 × 19.
;;
;; Find the first four consecutive integers to have four distinct primes
;; factors. What is the first of these numbers?
;;
;; Answer: 134043
;;

(load-file "factorize.clj")

(defn number->primes-count [n]
  (count (frequencies (factorize n))))

(defn p47 []
  (letfn [(bingo? [n]
            (and (every? (partial = 4) (map (fn [i] (number->primes-count (+ n i))) '(0 1 2 3)))
                 n))]
    (loop [n 5]
      (or (some bingo? (map (partial - n) '(3 2 1 0)))
        (recur (+ n 4))))))

;; end of file
