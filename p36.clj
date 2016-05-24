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


(defn palindromic? [n base]
  (let [digits   (number->digits n base)
        half-len (quot (count digits) 2)
        head     (take half-len digits)
        tail     (take half-len (reverse digits))]
    (= head tail)))


(defn p36 []
  (apply + (filter (fn [n] (and (palindromic? n 10) (palindromic? n 2))) 
                   (range 1 1000001))))

;; end of file
