;; atkin-sieve.scm
;;
;; determine primes up to given limit by Atkin/Bernstein
;; algorithm
;;
;; FIXME: It's not any faster than Sieve of Eratosthenes (sieve-primes.scm).
;; FIXME: How come?


(define primes-bitvector #f)


(define (prime? n)
  (and (> n 1)
       (< n (bitvector-length primes-bitvector))
       (bitvector-ref primes-bitvector n)))


(define (atkin-sieve limit)

  (define (flip-prime n)
    (bitvector-set! primes-bitvector n (not (bitvector-ref primes-bitvector n))))

  (define (not-a-prime n)
    (bitvector-set! primes-bitvector n #f))

  (set! primes-bitvector (make-bitvector (+ limit 1) #f))

  (for-each (lambda (n) (bitvector-set! primes-bitvector n #t)) '(1 2 3 4))

  (let ((sqr_lim (inexact->exact (floor (expt limit 0.5))))
        (x2 0)
        (y2 0)
        (n  0))

    (do ((x 1 (+ x 1))) ((> x sqr_lim))

        (set! x2 (* x x))

        (do ((y 1 (+ y 1))) ((> y sqr_lim))

            (set! y2 (* y y))

            (set! n (+ (* 4 x2) y2))

            (if (and (<= n limit)
                     (or (= 1 (remainder n 12))
                         (= 5 (remainder n 12))))
              (flip-prime n))

            (set! n (+ (* 3 x2) y2))

            (if (and (<= n limit) (= 7 (remainder n 12)))
              (flip-prime n))

            (set! n (- (* 3 x2) y2))

            (if (and (> x y) (<= n limit) (= 11 (remainder n 12)))
              (flip-prime n))))

    (do ((n 1 (+ n 1))) ((> n sqr_lim))

        (if (prime? n)

          (do ((nn (* n n) (+ nn (* n n)))) ((> nn limit))

              (not-a-prime nn))))))


;; end of file
