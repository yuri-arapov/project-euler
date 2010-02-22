;; primes-1000000.scp
;;
;; Primes under 1000000.


;; Load primes from file.
;;
(define (primes-1000000-load)

  (define (read-primes f)
    (let loop ((line (read-line f))
               (res  '()))
      (if (eof-object? line)
        (reverse res)
        (loop (read-line f) (cons (string->number line) res)))))

  (call-with-input-file "primes-1000000" read-primes))


;; List of primes under 1000000.
;;
(define primes-1000000 (primes-1000000-load))


;; Bitvector for fast primality test.
;;
(define primes-1000000-bv (make-bitvector 1000001 #f))
(for-each (lambda (n) (bitvector-set! primes-1000000-bv n #t)) primes-1000000)


;; Primality test.
;;
(define (primes-1000000-prime? n)
  (cond ((or (< n 2) (> n 1000000))
         #f)
        (else
          (bitvector-ref primes-1000000-bv n))))


;; end of file
