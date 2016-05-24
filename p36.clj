;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=36
;;
;; Problem 36
;; 31 January 2003
;;
;; The decimal number, 585 = 10010010012 (binary), is palindromic in both
;; bases.
;;
;; Find the sum of all numbers, less than one million, which are palindromic in
;; base 10 and base 2.
;;
;; (Please note that the palindromic number, in either base, may not include
;; leading zeros.) 
;;
;; Answer: 872187


(load-file "pe_utils.clj")


(defn same? [len s1 s2]
  (cond (zero? len) true
        (not (= (first s1) (first s2))) false
        :else (recur (dec len) (rest s1) (rest s2))))

(defn palindromic? [n base]
  (let [digits   (number->digits n base)]
    (same? (quot (count digits) 2) digits (reverse digits))))


(defn p36 []
  (apply + (filter (fn [n] (and (palindromic? n 10) (palindromic? n 2))) 
                   (range 1 1000001))))

;; end of file
