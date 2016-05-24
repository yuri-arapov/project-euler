;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=41
;;
;; Problem 41
;; 11 April 2003
;;
;; We shall say that an n-digit number is pandigital if it makes use of all the
;; digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
;; also prime.
;;
;; What is the largest n-digit pandigital prime that exists?
;;
;; Answer: 7652413


(load-file "pe_utils.clj")
(load-file "permutations.clj")
(load-file "prime.clj")

(defn p41
  ([number-of-digits]
   (->> (reverse (range 1 (inc number-of-digits))) ;; '(7 6 5 4 3 2 1)
        permutations                               ;; all possible digits combinations
        (map digits->number)                       ;; list all possible pandigital numbers
        (some (fn [n] (and (prime? n) n)))))       ;; first prime in the list
  ([]
   (p41 7)))


;; end of file
