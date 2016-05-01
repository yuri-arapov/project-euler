;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=10
;;
;; Problem 10
;; 08 February 2002
;;
;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
;;
;; Find the sum of all the primes below two million.
;; Answer: 142913828922
;;
;; FIXME: worth to try: http://en.wikipedia.org/wiki/Sieve_of_Atkin


(defn square [n]
  (* n n))

(defn prime? [n primes-so-far]
  (cond (empty? primes-so-far) true
        (zero? (rem n (first primes-so-far))) false
        (> (square (first primes-so-far)) n) true
        :else (recur n (rest primes-so-far))))

(defn p10 []
  (loop [n 3, primes [2]]
    (if (>= n 2000000) (apply + primes)
        (recur (+ n 2) (if (prime? n primes) (conj primes n) primes)))))

;; end of file
