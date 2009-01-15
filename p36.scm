;; 14 March 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=36
;;
;; Problem 36
;; 31 January 2003
;;
;; The decimal number, 585 = 10010010012 (binary), is palindromic in both
;; bases.
;;
;; Find the sum of all numbers, less than one million, which are palindromic in
;; base 10 and base 2.
;;
;; (Please note that the palindromic number, in either base, may not include
;; leading zeros.) 
;;
;; Answer: 872187


(load "accumulate.scm")


(define (palindromic? n base)
;; test if given number 'n' is palindromic in given radix 'base'
;;
  (let* ((ls       (string->list (number->string n base)))
         (half-len (quotient (length ls) 2)))
    (equal? (list-head ls half-len)
            (list-head (reverse ls) half-len))))


(define (p36 limit)
  (filtered-accumulate 
    +                                      ;; sum the numbers
    (lambda (n) n)                         ;; term: the number itself
    (lambda (n) (and (palindromic? n 10)   ;; number must be palindromic
                     (palindromic? n 2)))  ;;   in base 10 and 2
    (lambda (n) (+ n 2))                   ;; check odd numbers only
    0                                      ;; initial value of the sum
    1                                      ;; first value to test
    limit))                                ;; upper limit


(define (p36-old limit)
  (define (iter res n)
    (if (> n limit)
      res
      (iter (if (and (palindromic? n 10) (palindromic? n 2)) 
              (+ res n) 
              res) 
            (+ n 2)))) ;; stick to odd numbers
  (iter 0 1))


;; end of file
