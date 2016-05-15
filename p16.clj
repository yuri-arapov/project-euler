;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=16
;;
;; Problem 16
;; 03 May 2002
;;
;; 215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
;;
;;                                              1000
;; What is the sum of the digits of the number 2     ?
;;
;; Answer:
;;      1366


(load-file "pe_utils.clj")

(defn p16 []
  (apply + (number->digits (apply * (repeat 1000 2N)))))

;; end of file
