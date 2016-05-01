;;  Project Euler
;; 
;;  http://projecteuler.net/index.php?section=problems&id=7
;;  
;;  Problem 7
;;  28 December 2001
;; 
;;  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
;;  that the 6th prime is 13.
;; 
;;  What is the 10001st prime number?
;;
;;  Answer: 104743

(defn square [n] 
  (* n n))

(defn prime?  [n primes-so-far]
  (cond (empty? primes-so-far) true
        (zero? (rem n (first primes-so-far))) false
        (> (square (first primes-so-far)) n) true
        :else (recur n (rest primes-so-far))))

(defn p7 []
  (loop [n 3, primes [2], cnt 1]
    (if (prime? n primes)
      (if (= cnt 10000) n
          (recur (+ n 2) (conj primes n) (inc cnt)))
      (recur (+ n 2) primes cnt))))

;; end of file
