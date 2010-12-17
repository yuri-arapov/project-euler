;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=14
;;
;; Problem 14
;; 05 April 2002
;;
;; The following iterative sequence is defined for the set of positive integers:
;;
;;   n -> n/2    (n is even)
;;   n -> 3n + 1 (n is odd)
;;
;; Using the rule above and starting with 13, we generate the following sequence:
;; 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
;;
;; It can be seen that this sequence (starting at 13 and finishing at 1)
;; contains 10 terms. Although it has not been proved yet (Collatz Problem), it
;; is thought that all starting numbers finish at 1.
;;
;; Which starting number, under one million, produces the longest chain?
;;
;; NOTE: Once the chain starts the terms are allowed to go above one million.
;;
;; Answer: 837799


;; Hash-tables support
(use-modules (srfi srfi-69))


(define (p14)
  (let ((ht (make-hash-table)))

    (define (len-exists? n)
      (hash-table-exists? ht n))

    (define (len-ref n) 
      (and (hash-table-exists? ht n) (hash-table-ref ht n)))

    (define (len-set! n len)
      (hash-table-set! ht n len))

    (define (next n) (if (odd? n) 
                       (+ 1 (* n 3)) 
                       (/ n 2)))

    (define (chain-len n)
      (define (unwind-chain s len)
        (fold (lambda (n len) 
                (let ((ll (1+ len)))
                  (len-set! n ll)
                  ll))
              len
              s))
      (let loop ((n n)
                 (s '()))
        (if (len-exists? n)
          (unwind-chain s (len-ref n))
          (loop (next n) (cons n s)))))

    (len-set! 1 1)

    (let loop ((seed 1)
               (max-seed 0)
               (max-len 0))
      (if (= seed 1000000)
        (values max-seed max-len)
        (let ((l (chain-len seed)))
          (if (> l max-len)
            (loop (1+ seed) seed l)
            (loop (1+ seed) max-seed max-len)))))))


;; end of file
;; vim: ts=4 sw=4 et
