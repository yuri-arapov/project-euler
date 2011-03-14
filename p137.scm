;; 2011-03-14
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=137
;;
;; Problem 137
;;
;; 12 January 2007
;; 
;; Consider the infinite polynomial series AF(x) = xF1 + x^2F2 + x^3F3 + ...,
;; where Fk is the kth term in the Fibonacci sequence: 1, 1, 2, 3, 5, 8, ... ;
;; that is, Fk = Fk1 + Fk2, F1 = 1 and F2 = 1.
;; 
;; For this problem we shall be interested in values of x for which AF(x) is a
;; positive integer.
;; 
;; Surprisingly AF(1/2) = 2
;;
;; The corresponding values of x for the first five natural numbers are shown
;; below.
;; 
;; We shall call AF(x) a golden nugget if x is rational, because they become
;; increasingly rarer; for example, the 10th golden nugget is 74049690.
;; 
;; Find the 15th golden nugget.
;; 
;; Answer: 1120149658760
;;
;; Solution method.
;;
;;   The generating Fibonacci formula gives us:
;;            x
;;   n = -----------
;;       1 - x - x^2
;;
;;   As we're looking for rational arguments x to AF(x): x=a/b, where a,b
;;   positive integers, a<b.
;;
;;   The first golden nugget is 2 for x=1/2, so a=1, b=2
;;   The second golden nugget is 15 for a=3, b=5
;;   (determined manually here: http://www.alpertron.com.ar/QUAD.HTM)
;;
;;   It was noticed that 1-st golden nugget uses F2 and F3 as a and b,
;;   2-nd golden nugget uses F4 F5 (Fn -- n-th Fibonacci number).
;;
;;   The test of 10-th golden nugget confirmed that 10-th g.n. 
;;   uses F20 and F21.
;;
;;   So it was concluded that n-th g.n. must use F2n and F2n+1 Fibonacci 
;;   numbers as a and b.


;; Fibonacci generating function.
;; http://en.wikipedia.org/wiki/Fibonacci_number
(define (af x)
  (/ x (- 1 x (* x x))))


;; Return n-th and (n+1)-th Fibonacci numbers.
;; F0=0, F1=1, F2=1, F3=2, ...
(define (fib-pair n)
  (let loop ((n n) (a 0) (b 1))
    (if (zero? n) (values a b)
      (loop (1- n) b (+ a b)))))


;; Compute golden nugget (as defined above).
(define (golden-nugget n)
  (let-values (((f f+1) (fib-pair (* n 2))))
    (af (/ f f+1))))


(define (p137)
  (golden-nugget 15))


;; end of file
;; vim: sw=4 ts=4
