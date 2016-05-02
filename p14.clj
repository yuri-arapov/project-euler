;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=14
;;
;; Problem 14
;; 05 April 2002
;;
;; The following iterative sequence is defined for the set of positive integers:
;;
;;   n -> n/2    (n is even)
;;   n -> 3n + 1 (n is odd)
;;
;; Using the rule above and starting with 13, we generate the following sequence:
;; 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
;;
;; It can be seen that this sequence (starting at 13 and finishing at 1)
;; contains 10 terms. Although it has not been proved yet (Collatz Problem), it
;; is thought that all starting numbers finish at 1.
;;
;; Which starting number, under one million, produces the longest chain?
;;
;; NOTE: Once the chain starts the terms are allowed to go above one million.
;;
;; Answer: 837799

(defn next-n [n]
  (if (even? n)
    (/ n 2)
    (+ 1 (* n 3))))


(defn store-len [mp n len head]
  (let [new-mp (assoc mp n len)]
    (if (empty? head) new-mp
        (recur new-mp (first head) (inc len) (rest head)))))


(defn chain-length [n len-map]
  (loop [i n, head '()]
    (if-let [l (len-map i)]
      (let [new-len-map (store-len len-map i l head)]
        [new-len-map (new-len-map n)])
      (recur (next-n i) (cons i head)))))


(defn p14 []
  (loop [n 2, len-map {1 1}, [res-n res-len] [1 1]]
    (if (= n 1000000) [res-n res-len]
        (let [[new-len-map len] (chain-length n len-map)]
          (recur (inc n)
                 new-len-map
                 (if (> len res-len) [n len]
                     [res-n res-len]))))))


;; end of file
