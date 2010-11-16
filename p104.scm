;; 2010-08-19
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=104
;;
;; Problem 104
;; 09 September 2005
;;
;; The Fibonacci sequence is defined by the recurrence relation:
;;
;;     F(n) = F(n−1) + F(n−2), where F(1) = 1 and F(2) = 1.
;;
;; It turns out that F(541), which contains 113 digits, is the first
;; Fibonacci number for which the last nine digits are 1-9 pandigital
;; (contain all the digits 1 to 9, but not necessarily in order). And
;; F(2749), which contains 575 digits, is the first Fibonacci number for
;; which the first nine digits are 1-9 pandigital.
;;
;; Given that F(k) is the first Fibonacci number for which the first nine
;; digits AND the last nine digits are 1-9 pandigital, find k.
;;
;; Answer: 329468


(define (fib end-cond)
  (let loop ((k 0)
             (a 1) 
             (b 0))
;;    (if (zero? (remainder k 1000))
;;      (format #t "~a\n" k))
    (if (end-cond b)
      k
      (loop (1+ k) b (+ a b)))))


(define (pandigital? s) 
  (string=? "123456789" (list->string (sort (string->list s) char<?))))


(define (p104)
  (fib (lambda (n)
         (and-let* (((pandigital? (number->string (remainder n 1000000000))))
                    ((pandigital? (string-take (number->string n) 9))))))))

;; end of file
;; vim: ts=4 sw=4
