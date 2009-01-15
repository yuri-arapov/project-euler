;; sieve-primes.scm
;;
;; Sieve of Eratosthenes


(define primes-sieve-data false)


(define (prime? n)
  (and (> n 1)
       (< n (vector-length primes-sieve-data))
       (vector-ref primes-sieve-data n)))


(define (sieve-primes limit)

  (define (iter n)

    (define (strike-out x)
      (cond ((< x limit)
             (vector-set! primes-sieve-data x false)
             (strike-out (+ x n)))))

    (cond ((< n limit)
           (if (vector-ref primes-sieve-data n)
             (strike-out (+ n n)))
           (iter (+ n 1)))))

  (set! primes-sieve-data (make-vector limit true))
  (iter 2))


;; end of file
