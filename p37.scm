;; 14 March 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=37
;;
;; Problem 37
;;
;; 14 February 2003
;;
;; The number 3797 has an interesting property. Being prime itself, it is
;; possible to continuously remove digits from left to right, and remain prime
;; at each stage: 3797, 797, 97, and 7. Similarly we can work from right to
;; left: 3797, 379, 37, and 3.
;;
;; Find the sum of the only eleven primes that are both truncatable from left
;; to right and right to left.
;;
;; NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
;;
;; Answer:
;;      748317


(load "print.scm")
(load "accumulate.scm")
(load "sieve-primes.scm")


(define (right-left-trancatable? n)
  (if (< n 10)
    (prime? n)
    (and (prime? n) (right-left-trancatable? (quotient n 10)))))


(define (remove-leftmost-digit n)
  (string->number (list->string (cdr (string->list (number->string n 10))))))
;; FIXME: this must not be such lame...


(define (left-right-trancatable? n)
  (if (< n 10)
    (prime? n)
    (and (prime? n) (left-right-trancatable? (remove-leftmost-digit n)))))


(define (p37 limit)
  (println "sieving...")
  (sieve-primes limit)

  (println "filtering trancatable numbers...")
  (let ((res 
          (filtered-accumulate 
            (lambda (res n) (append res (list n)))         ;; op 
            (lambda (n) n)                                 ;; term 

            (lambda (n) (and (left-right-trancatable? n)   ;; filter
                             (right-left-trancatable? n))) ;;

            (lambda (n) (+ n 1))                           ;; next 
            '()                                            ;; init 
            11                                             ;; from 
            limit)))                                       ;; to

    (println res)
    (println (apply + res))))


;; FIXME: make some reasonable assumption about upper limit of the primes
;; FIXME: to be tested


;; end of file
