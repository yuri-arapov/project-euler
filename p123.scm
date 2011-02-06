;; 2011-02-06
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=123
;;
;; Problem 123
;; 16 June 2006
;;
;; Let pn be the nth prime: 2, 3, 5, 7, 11, ..., and let r be the remainder
;; when (pn-1)^n + (pn+1)^n is divided by pn^2.
;;
;; For example, when n = 3, p3 = 5, and 4^3 + 6^3 = 280 == 5 mod 25.
;;
;; The least value of n for which the remainder first exceeds 109 is 7037.
;;
;; Find the least value of n for which the remainder first exceeds 1010.
;;
;; Answer: 21035


(load "read-file.scm")


;; Observe this in maxima:
;;
;; (%i1) expand((a-1)^5+(a+1)^5);
;;                                 5       3
;; (%o1)                         2 a  + 20 a  + 10 a
;;
;; (%i2) expand((a-1)^6+(a+1)^6);
;;                               6       4       2
;; (%o2)                      2 a  + 30 a  + 30 a  + 2
;; 
;; So remainder r is 2 when n is even.
;;
;; For off n r = remainder(2na,a^2).
;;
(define (p123)
  (let ((primes (list->vector 
                  (read-file-with "first-1000000-primes" string->number))))
    (call/cc 
      (lambda (return) 
        (dorange-ex n 7037 1000000 2
          (let* ((p (vector-ref primes (1- n)))     ;; nth prime
                 (r (remainder (* 2 n p) (* p p)))) ;; nth remainder
            (if (> r 10000000000)
              (return n))))))))
;; end of file
;; vim: sw=4 ts=4
