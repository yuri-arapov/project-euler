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


(define (divider? n d) (zero? (remainder n d)))


(define (p10-int maxn)
  (let ((bv (make-bitvector (1+ maxn) #t)))

    (define (prime? pos) (bitvector-ref bv pos))

    (define (mark pos) (bitvector-set! bv pos #f))

    (define (mark-all pos)
      (dorange x 2 (quotient maxn pos)
        (mark (* pos x))))

    (format #t "ceiving...\n")
    (mark-all 2)
    (do ((n 3 (+ 2 n))) ((>= n maxn))
      (if (prime? n)
        (mark-all n)))

    (format #t "collecting...\n")
    (do ((n 3 (+ 2 n))
         (res 2 (if (prime? n) (+ res n) res)))
      ((>= n maxn) res))))


(define (p10) (p10-int 2000000))


;; end of file
;; vim: ts=4 sw=4 et
