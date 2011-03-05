;; 2011-03-05
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=130
;;
;; Problem 130
;; 27 October 2006
;;
;; A number consisting entirely of ones is called a repunit. We shall define
;; R(k) to be a repunit of length k; for example, R(6) = 111111.
;;
;; Given that n is a positive integer and GCD(n, 10) = 1, it can be shown that
;; there always exists a value, k, for which R(k) is divisible by n, and let
;; A(n) be the least such value of k; for example, A(7) = 6 and A(41) = 5.
;;
;; You are given that for all primes, p  5, that p  1 is divisible by A(p). For
;; example, when p = 41, A(41) = 5, and 40 is divisible by 5.
;;
;; However, there are rare composite values for which this is also true; the
;; first five examples being 91, 259, 451, 481, and 703.
;;
;; Find the sum of the first twenty-five composite values of n for which
;; GCD(n, 10) = 1 and n  1 is divisible by A(n).
;;
;; Answer: 149253


;; Find multiplier m so that last digit of n*m+x is 1.
(define (find-multiplier n x)
  (find (lambda (m) (= 1 (remainder (+ x (* n m)) 10))) '(0 1 2 3 4 5 6 7 8 9)))


;; Map of mutipliers.  See find-multiplier.
;; Obtained as:
;;   (map 
;;     (lambda (n) 
;;       (map (curry find-multiplier n) 
;;            '(0 1 2 3 4 5 6 7 8 9))) 
;;     '(0 1 2 3 4 5 6 7 8 9))
;;
(define *mul*
  ; n 0  1  2  3  4  5  6  7  8  9
                                       ; x
'#(#(#f  0 #f #f #f #f #f #f #f #f)    ; 0
   #( 1  0  9  8  7  6  5  4  3  2)    ; 1
   #(#f  0 #f  4 #f  3 #f  2 #f  1)    ; 2
   #( 7  0  3  6  9  2  5  8  1  4)    ; 3
   #(#f  0 #f  2 #f  4 #f  1 #f  3)    ; 4
   #(#f  0 #f #f #f #f  1 #f #f #f)    ; 5
   #(#f  0 #f  3 #f  1 #f  4 #f  2)    ; 6
   #( 3  0  7  4  1  8  5  2  9  6)    ; 7
   #(#f  0 #f  1 #f  2 #f  3 #f  4)    ; 8
   #( 9  0  1  2  3  4  5  6  7  8)))  ; 9

(define (mul n x)
  (vector-ref (vector-ref *mul* (remainder n 10)) 
              (remainder x 10)))


;; Test number for being repunit.
(define (repunit? n)
  (let loop ((n n))
    (cond ((= n 1) #t)
          ((not (= 1 (remainder n 10))) #f)
          (else (loop (quotient n 10))))))


;; Determine number of digits of n.
(define (number-of-digits n)
;  (inexact->exact (ceiling (log10 (1+ n)))))
  (if (zero? n)
    0
    (1+ (number-of-digits (quotient n 10)))))


;; Function A(n) -> k.
(define (A n)
  (let loop ((x 0) (count 0))
    (if (not (mul n x))
      #f
      (let ((y (+ x (* n (mul n x)))))
        (if (repunit? y)
          (+ count (number-of-digits y))
          (loop (quotient y 10) (1+ count)))))))



(define (divisor? n d) (zero? (remainder n d)))


;; Sloane A005939: http://oeis.org/A005939
(define base10-pseudoprimes '(
  9 33 91 99 259 451 481 561 657 703 909 1233 1729 
  2409 2821 2981 3333 3367 4141 4187 4521 5461 6533 
  6541 6601 7107 7471 7777 8149 8401 8911 10001 
  11111 11169 11649 12403 12801 13833 13981 14701 
  14817 14911 15211))


;; Solve problem 130.
;; The search for first 5 magic numbers given in problem definitions --
;; 91, 259, 451, 481, and 703 -- showed that they are all pseudoprimes
;; base 10 (see above).  So I filter base 10 pseudoprimes to fetch
;; only those that fit the problem criterion.
(define (p130)
  (apply +
         (take
           (filter
             (lambda (n)
               (let ((k (A n)))
                 (and k (divisor? (1- n) k))))
             base10-pseudoprimes)
           25)))


;; end of file
;; vim: sw=4 ts=4
