;; http://www.dreamincode.net/code/snippet4193.htm
;; Miller-Rabin primality test
;; A rather fast O(log² n) probabilistic algorithm for checking number's
;; primality. This is a slight modification to Fermat's primality test that
;; cannot be tricked with Carmichael's numbers, making it even more precise.

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
  (if (<= n 7)
    (list? (member n '(2 3 5 7)))
    (prime? n 20)))

;; end of file
