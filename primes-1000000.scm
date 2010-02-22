;; primes-1000000.scp
;;
;; Primes under 1000000.


;; Load primes from file
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


;; end of file
