;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=20
;;
;; Problem 20
;; 21 June 2002
;;
;; n! means n x (n - 1) x ... x 3 x 2 x 1
;;
;; Find the sum of the digits in the number 100!
;;
;; Answer:
;;      648


;; number->digits
(load-file "pe_utils.clj")


(defn fst [s] (if (empty? s) 0 (first s)))
(defn nxt [s] (if (empty? s) s (rest s)))


;; sum of two numbers represented as lists of decimal digits.
;; (sum '(1 2 3) '(4 5 6)) => (5 7 9)
(defn sum [x y]
  (loop [res '()
         shift 0
         x (reverse x)
         y (reverse y)]
    (if (and (empty? x) (empty? y) (zero? shift)) res
      (let [ss (+ shift (fst x) (fst y))]
        (recur (cons (rem ss 10) res) (quot ss 10) (nxt x) (nxt y))))))


;; product of two numbers represented as lists of decimal digits.
(defn mul [x y]
  (let [mul1 (fn [x digit pos]
               (loop [res (repeat pos 0)
                      shift 0
                      x (reverse x)]
                 (if (and (empty? x) (zero? shift)) res
                   (let [ss (+ shift (* digit (fst x)))]
                     (recur (cons (rem ss 10) res) (quot ss 10) (nxt x))))))]
    (loop [res '()
           pos 0
           yy (reverse y)]
      (if (empty? yy) res
        (recur (sum res (mul1 x (first yy) pos)) (inc pos) (rest yy))))))


;; compute n!, return list of decimal digits.
(defn fac [n]
  (loop [res '(1), x 2]
    (if (= x n) res
      (recur (mul res (number->digits x)) (inc x)))))


(defn p20 []
  (apply + (fac 100)))


;; end of file

