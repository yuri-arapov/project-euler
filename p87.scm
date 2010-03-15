;; 2010-03-14
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=87
;;
;; Problem 87
;; 21 January 2005
;;
;; The smallest number expressible as the sum of a prime square, prime cube,
;; and prime fourth power is 28. In fact, there are exactly four numbers below
;; fifty that can be expressed in such a way:
;;
;; 28 = 2^2 + 2^3 + 2^4
;; 33 = 3^2 + 2^3 + 2^4
;; 49 = 5^2 + 2^3 + 2^4
;; 47 = 2^2 + 3^3 + 2^4
;;
;; How many numbers below fifty million can be expressed as the sum of a prime
;; square, prime cube, and prime fourth power?
;;
;; Answer: 1097343
;;


(load "primes-1000000.scm") ;; primes under 1000000
(load "uniq.scm")           ;; unduper


(define primes primes-1000000)  ;; shortcut


;; Transform initial sequence with given fun until resultant elements do not
;; exceed given limit.
;;
(define (make-list-limited src fun limit)
  (let loop ((s src) (res '()))
    (if (null? src)
      (reverse res)
      (let ((x (fun (car s))))
        (if (>= x limit)
          (reverse res)
          (loop (cdr s) (cons x res)))))))


(define (pow2 x) (* x x))           ;; x^2
(define (pow3 x) (* x x x))         ;; x^3
(define (pow4 x) (pow2 (pow2 x)))   ;; x^4


;; Problem's limit.
;;
(define limit 50000000)


;; Return unsorted and non-unduped list of numbers that match with problem 87
;; definition.
;;
(define (p87-int)
  (let ((ps2 (make-list-limited primes pow2 limit))
        (ps3 (make-list-limited primes pow3 limit))
        (ps4 (make-list-limited primes pow4 limit)))

    (for-each (lambda (s)
                (format #t "~a ... ~a\n" (length s) (take-right s 5)))
              (list ps2 ps3 ps4))

    (let loop ((s2 ps2)
               (s3 ps3)
               (s4 ps4)
               (res '()))
      (cond
        ((null? s2)     res)
        ((null? s3)     (loop (cdr s2) ps3 ps4 res))
        ((null? s4)     (loop s2 (cdr s3) ps4 res))
        (else
          (let ((x (+ (car s2) (car s3) (car s4))))
            (if (< x limit)
              (loop s2 s3       (cdr s4) (cons x res))
              (loop s2 (cdr s3) ps4      res))))))))


;; Problem 87.
;;
(define (p87)
  (length (uniq (sort (p87-int) <))))


;; end of file
;; vim: sw=4 ts=4
