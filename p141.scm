;; 2011-10-04
;; 2015-02-07
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=141
;;
;; Problem 141
;;
;;17 February 2007
;; 
;; A positive integer, n, is divided by d and the quotient and remainder are q
;; and r respectively. In addition d, q, and r are consecutive positive integer
;; terms in a geometric sequence, but not necessarily in that order.
;; 
;; For example, 58 divided by 6 has quotient 9 and remainder 4. It can also be
;; seen that 4, 6, 9 are consecutive terms in a geometric sequence (common
;; ratio 3/2).
;; We will call such numbers, n, progressive.
;; 
;; Some progressive numbers, such as 9 and 10404 = 102^2, happen to also be
;; perfect squares.
;; The sum of all progressive perfect squares below one hundred thousand is
;; 124657.
;; 
;; Find the sum of all progressive perfect squares below one trillion (10^12).
;;
;; Answer: 878454337159


(use-modules (srfi srfi-69))    ;; Guile's hash tables


(load "integer-square-root.scm")

;; generate progressive numbers that do exceed given 'limit'.
;; pass each progressive number to (consumer n res) function
;; where 'n' is progressive number and 'res' is first initialized with 'init'
;; and then replaced with result of 'consumer' function.
;; return result of (end res) function.
(define (generate-progressive-numbers limit consumer init end)
  (let loop ((a 2) (b 1) (m 1) (res init))
    (cond ((> (* a a a) limit) (end res))
          ((= b a) (loop (1+ a) 1 1 res))
          ((not (= 1 (gcd a b))) (loop a (1+ b) m res))
          (else
            (let ((n (+ (* a a a m m b) (* b b m))))
              (cond ((> n limit) (loop a (1+ b) 1 res))
                    (else 
                      (loop a b (1+ m) (consumer n res)))))))))


;; generate raw list of progressive numbers up to given 'limit'.
(define (simple-progressive-test limit)
  (generate-progressive-numbers 
    limit
    (lambda (n res) (cons n res))
    '()
    identity))



;; problem solver
(define (p141-limit limit)
  (format #t "problem 141, upper limit ~a\n" limit)
  (generate-progressive-numbers
    limit

    ;; consumer
    (lambda (n res) (hash-table-set! res n n) res)

    ;; init
    (make-hash-table =)

    ;; terminator
    (lambda (res) 
      (format #t "~a progressive numbers. filtering squares and summing up...\n" 
              (hash-table-size res))
      (apply + (filter square? (map car (hash-table->alist res)))))))


;; problem solver
(define (p141)
  (p141-limit (expt 10 12)))


;; end of file
;; vim: sw=4 ts=4
