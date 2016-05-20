;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=35
;;
;; Problem 35
;; 17 January 2003
;;
;; The number, 197, is called a circular prime because all rotations of the
;; digits: 197, 971, and 719, are themselves prime.
;;
;; There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37,
;; 71, 73, 79, and 97.
;;
;; How many circular primes are there below one million?
;;
;; Answer: 55

(load-file "prime.clj")


(defn number-of-digits [n]
  (if (< n 10) 1
      (inc (number-of-digits (quot n 10)))))


(defn prime-rotations [n]
  (let [len (number-of-digits n)
        mul (apply * (repeat (dec len) 10))
        rotate (fn [x] (+ (quot x 10) (* (rem x 10) mul)))]
    (loop [res '(), iter len, x n]
      (cond (zero? iter) res
            (not (prime? x)) '()
            :else (recur (conj res x) (dec iter) (rotate x))))))


(defn p35 []
  (count
   (reduce
    (fn [res n]
      (if (res n) res
          (reduce conj res (prime-rotations n))))
    #{2}
    (range 3 1000000 2))))

;; end of file
