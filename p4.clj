;; April 29, 2016
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; Problem 4
;; http://projecteuler.net/index.php?section=problems&id=4
;; 
;; A palindromic number reads the same both ways. The largest
;; palindrome made from the product of two 2-digit numbers is
;; 9009 = 91 x 99.
;; 
;; Find the largest palindrome made from the product of two
;; 3-digit numbers.
;; 
;; Answer: 906609
;; 

(defn number->digits [n]
  (if (zero? n) '(0)
    (loop [res '(), n n]
      (if (zero? n) res
        (recur (cons (rem n 10) res) (quot n 10))))))

(defn digits->number [digits]
  (reduce (fn [res n] (+ (* res 10) n)) 0 digits))

(defn palindrome? [n]
  (let [digits (number->digits n)
        half-len (quot (count digits) 2)
        l (digits->number (take half-len digits))
        r (digits->number (take half-len (reverse digits)))]
    (= l r)))

(defn p4 []
  (loop [n1 999, n2 999, res 0]
    (cond (< n1 100) res
          (< n2 100) (recur (dec n1) (dec n1) res)
          :else
            (let [nn (* n1 n2)]
              (recur n1 (dec n2) (if (and (> nn res) (palindrome? nn)) nn res))))))

;; end of file
;; vim: et
