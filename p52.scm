;; 01 April 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=52
;;
;; Problem 52
;; 12 September 2003
;;
;; It can be seen that the number, 125874, and its double, 251748, contain
;; exactly the same digits, but in a different order.
;;
;; Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
;; contain the same digits.
;;
;; Answer: 142857
;;
;; FIXME: bruteforce


(load "print.scm")


(define (number->digits n)
  (if (<  n 10)
    (list n)
    (cons (remainder n 10) (number->digits (quotient n 10)))))


(define (sort-digits dd)
  (sort-list dd (lambda (x y) (< x y))))


(define (same-digits? a b)
  (equal? (sort-digits (number->digits a))
          (sort-digits (number->digits b))))


(define (matching-numbers? a . b)
  (reduce (lambda (i res) (and i res)) 
          true 
          (map (lambda (n) (same-digits? a n)) b)))
;; FIXME: make use of call/cc to stop testing
;; FIXME: on first mismatch


(define (p52)
  (let loop ((n 1))
;;    (if (zero? (remainder n 1000))
;;      (println n))
    (if (matching-numbers? n (* n 2) (* n 3) (* n 4) (* n 5) (* n 6))
      n
      (loop (+ n 1)))))


;; end of file
