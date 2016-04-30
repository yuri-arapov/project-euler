

(load "pollard-rho.scm")
(load "miller-rabin-primality-test.scm")

(define (factor n) (pollard prime? brent n))
