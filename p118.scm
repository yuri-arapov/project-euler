;; 2010-12-28
;;
;; Project Euler
;;
;; Problem 118
;; 24 March 2006
;;
;; Using all of the digits 1 through 9 and concatenating them freely to form
;; decimal integers, different sets can be formed. Interestingly with the set
;; {2,5,47,89,631}, all of the elements belonging to it are prime.
;;
;; How many distinct sets containing each of the digits one through nine
;; exactly once contain only prime elements?
;;
;; Answer: 


(use-modules (srfi srfi-11)) ;; (let-values ...)


(load "miller-rabin-primality-test.scm")

(define (cut-prime prev s)
  (let loop ((n (car s))
             (s (cdr s)))
    (cond 
      ((and (> n prev) (prime? n)) (values n s))
      ((null? s) (values #f #f))
      (else (loop (+ (* n 10) (car s)) (cdr s))))))


(define (digits->primes s)
  (let loop ((s s)
             (res '()))
    (if (null? s)
      (reverse res)
      (let-values (((n s) (cut-prime (if (null? res) 0 (car res)) s)))
        (if n
          (loop s (cons n res))
          #f)))))


;; end of file
;; vim: ts=4 sw=4 et
