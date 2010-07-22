;; 2010-07-18
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=95
;;
;; Problem 95
;; 13 May 2005
;;
;; The proper divisors of a number are all the divisors excluding the number
;; itself. For example, the proper divisors of 28 are 1, 2, 4, 7, and 14. As
;; the sum of these divisors is equal to 28, we call it a perfect number.
;;
;; Interestingly the sum of the proper divisors of 220 is 284 and the sum of
;; the proper divisors of 284 is 220, forming a chain of two numbers. For this
;; reason, 220 and 284 are called an amicable pair.
;;
;; Perhaps less well known are longer chains. For example, starting with 12496,
;; we form a chain of five numbers:
;;
;; 12496 -> 14288 -> 15472 -> 14536 -> 14264 (-> 12496 -> ...)
;;
;; Since this chain returns to its starting point, it is called an amicable
;; chain.
;;
;; Find the smallest member of the longest amicable chain with no element
;; exceeding one million.
;;
;; Answer: 14316


(load "permutations.scm")
(load "uniq.scm")
(load "range.scm")

(load "primes-1000000.scm")
(define primes primes-1000000)
(define prime? primes-1000000-prime?)


;; Merge sorted lists skipping duplicates.
;;
(define (merge s1 s2)
  (let loop ((s1 s1)
             (s2 s2)
             (res '()))
    (cond 
      ((null? s1)
       (append (reverse res) s2))
      ((null? s2)
       (append (reverse res) s1))
      ((< (car s1) (car s2))
       (loop (cdr s1) s2 (cons (car s1) res)))
      ((< (car s2) (car s1))
       (loop s1 (cdr s2) (cons (car s2) res)))
      (else
        ;; (car s1) equals to (car s2)
        (loop (cdr s1) (cdr s2) (cons (car s1) res))))))


;; Multiply each list element with given p.
;;
(define (mul s p)
  (map (lambda (n) (* n p)) s))


;; Make a function that is to compute proper divisors of the number
;; up to given limit.
;;
;; (make-proper-divisors-solver limit) -> (lambda (n) ...) -> (a b c ...)
;;
(define (make-proper-divisors-solver limit)
  (let ((pd-vec (list->vector (make-list (1+ limit)))) ;; memoization is here
        (verbose #f)) ;; set #t for debugging
    (define (pd-fun n)
      (if verbose
        (format #t "pd-fun ~a\n" n))
      (if (or (negative? n) (= 0 n) (= 1 n) (> n limit))
        '()
        (let ((x (vector-ref pd-vec n)))
          (if (not (null? x))
            x
            (let ((res
                    (if (prime? n)
                      '(1)
                      (let* ((first-prime-divisor (find (lambda (p) 
                                                          (zero? (remainder n p))) 
                                                        primes))
                             (q (quotient n first-prime-divisor))
                             (q-pd (pd-fun q))
                             (res (merge (merge q-pd
                                                (mul q-pd first-prime-divisor))
                                         (list q))))
                        res))))
              (if verbose
                (format #t "pd-fun ~a -> ~a\n" n res))
              (vector-set! pd-vec n res)
              res)))))
    pd-fun))


;; Make a function to compute list of amicable numbers.
;;
;; (make-amicable-chain-solver limit) -> (lambda (n) ...) -> (a b c ...)
;;
(define (make-amicable-chain-solver limit)

  (let ((number->proper-divisors (make-proper-divisors-solver limit))
        (amicable-chains (make-vector (1+ limit) #f))) ;; memoization is here

    (define (get n)
      (if (> n limit)
        '()
        (vector-ref amicable-chains n)))

    (define (store-and-result ls res)
      (for-each (lambda (i)
                  (if (and (<= i limit) (not (vector-ref amicable-chains i)))
                    (vector-set! amicable-chains i res)))
                ls)
      res)

    (lambda (n)
      (let loop ((n n)
                 (s (list n)))
        (if (get n)
          (store-and-result s (get n))
          (let* ((next (apply + (number->proper-divisors n)))
                 (ind (list-index (lambda (i) (= i next)) s)))
            (if (not ind)
              (loop next (cons next s))
              (store-and-result s (reverse (take s (1+ ind)))))))))))
  

;; Solve problem 95 in given limit.
;;
(define (p95-limit limit)
  (let ((number->amicable-chain (make-amicable-chain-solver limit)))
    (let loop ((n 1)
               (res '()))
      (if (zero? (remainder n 10000))
        (format #t "~a " n))
      (if (> n limit)
        (begin
          (format #t "\n~a\n" res)
          (apply min res))
        (let ((x (number->amicable-chain n)))
          (loop (1+ n)
                (if (> (length x) (length res))
                  x
                  res)))))))


;; Solve problem 95.
;;
(define (p95) (p95-limit 1000000))


;; end of file
;; vim: ts=4 sw=4
