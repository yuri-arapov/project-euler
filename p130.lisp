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


(load "lisp-utils.lisp")


;; Find multiplier m so that last digit of n*m+x is 1.
;;(defun find-multiplier (n x)
;;  (find-if '#(lambda (m) (= 1 (rem (+ x (* n m)) 10))) '(0 1 2 3 4 5 6 7 8 9)))


;; Map of mutipliers.  See find-multiplier.
;; Obtained as:
;;   (map 
;;     (lambda (n) 
;;       (map (curry find-multiplier n) 
;;            '(0 1 2 3 4 5 6 7 8 9))) 
;;     '(0 1 2 3 4 5 6 7 8 9))
;;
(defvar *mul*
  ; n  0   1   2   3   4   5   6   7   8   9
                                                ; x
'#(#(nil   0 nil nil nil nil nil nil nil nil)   ; 0
   #(  1   0   9   8   7   6   5   4   3   2)   ; 1
   #(nil   0 nil   4 nil   3 nil   2 nil   1)   ; 2
   #(  7   0   3   6   9   2   5   8   1   4)   ; 3
   #(nil   0 nil   2 nil   4 nil   1 nil   3)   ; 4
   #(nil   0 nil nil nil nil   1 nil nil nil)   ; 5
   #(nil   0 nil   3 nil   1 nil   4 nil   2)   ; 6
   #(  3   0   7   4   1   8   5   2   9   6)   ; 7
   #(nil   0 nil   1 nil   2 nil   3 nil   4)   ; 8
   #(  9   0   1   2   3   4   5   6   7   8))) ; 9

(defun mul (n x)
  (aref (aref *mul* (rem n 10)) 
        (rem x 10)))


(defun quotient (n d) (truncate (/ n d)))


;; Test number for being repunit.
(defun repunitp (n)
  (labels ((iter (n)
             (cond ((= n 1)                 t)
                   ((not (= 1 (rem n 10)))  nil)
                   (t                       (iter (quotient n 10))))))
    (iter n)))


;; Determine number of digits of n.
(defun number-of-digits (n)
  (if (zerop n)
    0
    (1+ (number-of-digits (quotient n 10)))))


;; Function A(n) -> k.
(defun A (n)
  (labels ((iter (x count)
             (if (not (mul n x))
               nil
               (let ((y (+ x (* n (mul n x)))))
                 (if (repunitp y)
                   (+ count (number-of-digits y))
                   (iter (quotient y 10) (1+ count)))))))
    (iter 0 0)))


(defun divisorp (n d) (zerop (rem n d)))


;; Sloane A005939: http://oeis.org/A005939
(defvar base10-pseudoprimes '(
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
(defun p130 ()
  (apply #'+
         (take
           (remove-if-not ; aka filter
             #'(lambda (n)
               (let ((k (A n)))
                 (and k (divisorp (1- n) k))))
             base10-pseudoprimes)
           25)))


;; end of file
;; vim: sw=4 ts=4
