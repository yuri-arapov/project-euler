;; 2011-02-06
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=124
;;
;; Problem 124
;; 14 July 2006
;; 
;; The radical of n, rad(n), is the product of distinct prime factors of n. For
;; example, 504 = 2^3 x 3^2 x 7, so rad(504) = 2 x 3 x 7 = 42.
;; 
;; If we calculate rad(n) for 1<=n<=10, then sort them on rad(n), and sorting
;; on n if the radical values are equal, we get:
;; 
;;  Unsorted                 Sorted
;;  n     rad(n)          n     rad(n)  k
;;  1      1              1      1      1
;;  2      2              2      2      2
;;  3      3              4      2      3
;;  4      2              8      2      4
;;  5      5              3      3      5
;;  6      6              9      3      6
;;  7      7              5      5      7
;;  8      2              6      6      8
;;  9      3              7      7      9
;; 10     10             10     10     10
;;
;; Let E(k) be the kth element in the sorted n column; for example, E(4) = 8
;; and E(6) = 9.
;; 
;; If rad(n) is sorted for 1<=n<=100000, find E(10000).
;; 
;; Answer: 21417


(load "range.scm")
(load "pollard-rho.scm")
(load "miller-rabin-primality-test.scm")
(load "uniq.scm")


;; Factoring of n.
;; Pollard Rho, Brent cycle-finding, Miller-Rabin primality test.
(define (factor n) (pollard prime? brent n))


;; Radical of n.
(define (rad n) (apply * (uniq (factor n))))


(define (p124)
  (list-ref
    (sort 
      (map (lambda (n) (cons n (rad n))) (range 1 100000))
      (lambda (i j)
        (let ((n1 (car i)) (r1 (cdr i))
              (n2 (car j)) (r2 (cdr j)))
          (or (< r1 r2) (and (= r1 r2) (< n1 n2))))))
    (1- 10000)))


;; end of file
;; vim: sw=4 ts=4
