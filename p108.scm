;; 2010-09-02
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=108
;;
;; Problem 108
;; 04 November 2005
;;
;; In the following equation x, y, and n are positive integers.
;;
;;   1   1   1
;;   - + - = -
;;   x   y   n
;;
;; For n = 4 there are exactly three distinct solutions:
;;
;;   1   1    1
;;   - + -- = -
;;   5   20   4
;;
;;   1   1    1
;;   - + -- = -
;;   6   12   4
;;
;;   1   1   1
;;   - + - = -
;;   8   8   4
;;
;; What is the least value of n for which the number of distinct solutions
;; exceeds one-thousand?
;;
;; NOTE: This problem is an easier version of problem 110; it is strongly
;; advised that you solve this one first.
;;
;; Answer: 180180
;;
;; FIXME: try and guess method



(load "range.scm")
(load "product.scm")
(load "combinations.scm")


(define (print s) (for-each (lambda (i) (format #t "~a\n" i)) s))


(define (product-all s)
  (if (null? s)
    '()
    (let loop ((s (cdr s))
               (res (car s)))
      (if (null? s)
        res
        (loop (cdr s) (product res (car s)))))))


(define (all-combinations s)
  (let loop ((n (length s))
             (res '()))
    (if (zero? n)
      res
      (loop (1- n) (cons (combinations s n) res)))))


(define (flatten s) (apply append s))


(define (factors->all-divisors s)
  (flatten
    (map (lambda (i) (map (lambda (j) (if (list? j) (apply * j) j)) i))
         (map flatten
              (map (lambda (i) (map product-all i))
                   (all-combinations s))))))


;; testing routine
;;
(define (xxx)
  (factors->all-divisors '((2 4 8) (3 9) (5 25))))


;; testing routine
;;
(define (number->divisors-slow n)
  (let loop ((d 1)
             (res '()))
    (if (> (* d d) n)
      res
      (if (zero? (remainder n d))
        (loop (1+ d) (cons d res))
        (loop (1+ d) res)))))


(define (n-x->y n x)    (/ (* x n) (- x n)))

(define (min-x n)       (+ 1 n))
(define (max-x n)       (* 2 n))


(define (distinct-solutions n)
  (filter integer? (map (lambda (x) (n-x->y n x))
                        (range (min-x n) (max-x n)))))



(define (test n)
  (map (lambda (y) (cons y (n-x->y n y))) (distinct-solutions n)))


;; testing routine
;;
(define (foo s)
  (= (number-of-ds s) (length (distinct-solutions (factors->n (make-factors s))))))



;; s is a ((2 3) (3 1) ...) -like list
;;        | |
;;        | divisor's power
;;        |
;;        prime divisor
;;
;; (make-factors '((2 3) (3 1))) -> ((8 4 2) (3))
;;
(define (make-factors s)
  (map (lambda (i)
         (let ((divisor (car i))
               (power   (cadr i)))
           (let loop ((n 1)
                      (res (list divisor)))
             (if (= n power)
               res
               (loop (1+ n) (cons (* divisor (car res)) res))))))
       s))


;; compute number from series of factors this number is made of
;;
;; higher powers must go first:
;; s must be ((8 4 2) (9 3)) -like list, 
;; this is what (make-factors ...) provides.
;;
(define (factors->n s) (apply * (map car s)))
  

;; square the number written as series of primes and their powers.
;; exmaple:
;;   (sauqre '((2 1) (3 2))) -> ((2 2) (3 4))
;;
(define (square s)
  (map (lambda (i) (list (car i) (* 2 (cadr i)))) s))



;; compute number of distinct solutions for the number n written as 
;; sequence of primes and their powers.
;; example:
;;   (number-of-ds '((2 1) (3 1))) ->
;;   5   ;; number of distinct solutions
;;   6   ;; the n itself
;;
(define (number-of-ds ns)
  (let ((n (factors->n (make-factors ns))))
    (values 
      (1+ (length 
            (filter 
              (lambda (i) (<= i n)) 
              (factors->all-divisors (make-factors (square ns))))))
      n)))


;; solve problem 108
;; FIXME: manually found solution
;;
(define (p108)
  (number-of-ds '((2 2) (3 2) (5 1) (7 1) (11 1) (13 1))))


;; end of file
;; vim: ts=4 sw=4
