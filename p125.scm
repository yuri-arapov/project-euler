;; 2011-02-07
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=125
;;
;; Problem 125
;; 04 August 2006
;;
;; The palindromic number 595 is interesting because it can be written as the
;; sum of consecutive squares: 6^2 + 7^2 + 8^2 + 9^2 + 10^2 + 11^2 + 12^2.
;;
;; There are exactly eleven palindromes below one-thousand that can be written
;; as consecutive square sums, and the sum of these palindromes is 4164. Note
;; that 1 = 0^2 + 1^2 has not been included as this problem is concerned with the
;; squares of positive integers.
;;
;; Find the sum of all the numbers less than 10^8 that are both palindromic and
;; can be written as the sum of consecutive squares.
;;
;; Answer: 2906969179


(load "uniq.scm")


;; Determine number of decimal digits of the given number.
(define (numlen n) 
  (inexact->exact (round (ceiling (log10 (1+ n))))))


;; Test if given number is palindromic.
(define (palindromic? n)
  (let ((len (numlen n)))
    (let loop ((n n) (l len) (hi (expt 10 (1- len))))
      (cond ((or (zero? n) (< l 2))
             #t)
            ((= (quotient n hi) (remainder n 10)) 
             (loop (quotient (remainder n hi) 10)
                   (- l 2)
                   (quotient hi 100)))
            (else
              #f)))))


;; Make list of squares up to given limit.
(define (make-squares limit)
  (let loop ((n 1) (res '()))
    (if (> (* n n) limit)
      (reverse res)
      (loop (1+ n) (cons (* n n) res)))))


;; Solution of problem 125 (problem limit is customized).
(define (p125-ex limit)

  (define (helper x s res)
    (if (or (>= x limit) (null? s))
      res
      (helper (+ x (car s)) (cdr s) (cons x res))))

  (define (iter s res)
    (if (null? (cdr s))
      res
      (iter (cdr s) (helper (+ (car s) (cadr s)) (cddr s) res))))

  (apply + (uniq (sort (filter palindromic?  (iter (make-squares limit) '()))
                       <))))


(define (p125)
  (p125-ex (expt 10 8)))


;; end of file
;; vim: sw=3 ts=4
