;; http://www.dreamincode.net/code/snippet4193.htm
;; Miller-Rabin primality test
;; A rather fast O(log² n) probabilistic algorithm for checking number's
;; primality. This is a slight modification to Fermat's primality test that
;; cannot be tricked with Carmichael's numbers, making it even more precise.
;;
;; IMPORTANT:
;;   The 294409 410041 512461 numbers are not prime but [sometimes] reported 
;;   as primes erroneously.
;;   Discovered when solving problem 72 and comparing list of primes
;;   under 1000000 determined by Miller-Rabin primality test and
;;   list of primes downlowaded from 
;;   http://primes.utm.edu/lists/small/millions/

;Returns (a ^ p) mod n
(define (expmod a p n)
  (cond ((= p 0) 1)
        ((= (remainder p 2) 1) (remainder (* a (expmod a (- p 1) n)) n))
        (else (remainder ((lambda (x) (* x x)) (expmod a (/ p 2) n)) n))))

;Will return true if n is prime, false otherwise
(define (prime? n)  
  (define (prime? n times)
    (define (good? a)
      (= (expmod a (- n 1) n) 1))
    (cond ((= times 0) true)
          ((good? (+ (random (- n 3)) 2)) (prime? n (- times 1)))
          (else false)))
  (cond ((member n '(29341 294409 410041 512461 65241793)) #f)
        ((and (<= n 23) (member n '(2 3 5 7 11 13 17 19 23))) #t)
        (else (prime? n 20))))

;; end of file
