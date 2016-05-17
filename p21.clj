;; Projet Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=21
;;
;; Problem 21
;; 05 July 2002
;;
;; Let d(n) be defined as the sum of proper divisors of n (numbers less than n
;; which divide evenly into n).
;;
;; If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and
;; each of a and b are called amicable numbers.
;;
;; For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44,
;; 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4,
;; 71 and 142; so d(284) = 220.
;;
;; Evaluate the sum of all the amicable numbers under 10000.
;;
;; Answer:
;;      31626

(defn D [n]
  (let [X (+ 1 (Math/sqrt n))]
    (loop [s 1, d 2]
      (if (>= d X) s
          (let [s1 (if (zero? (rem n d)) d 0)
                s2 (if (and  (zero? (rem n d)) (< (* d d) n)) (/ n d) 0)]
            (recur (+ s s1 s2) (inc d)))))))

(defn p21 []
  (loop [s 0, a 1]
    (if (= a 10000) (/ s 2)
        (let [b (D a)
              x (D b)]
          (recur (if (and (not (= a b)) (= a x)) (+ s a b) s) (inc a))))))

;; end of file
