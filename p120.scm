;; 2011-02-02
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=120
;;
;; Problem 120
;; 21 April 2006
;;
;; Let r be the remainder when (a-1)^n + (a+1)^n is divided by a^2.
;;
;; For example, if a = 7 and n = 3, then r = 42: 6^3 + 8^3 = 728 == 42 mod 49. And
;; as n varies, so too will r, but for a = 7 it turns out that rmax = 42.
;;
;; For 3 <= a <= 1000, find  SUM(rmax).
;;
;; Answer: 333082500


(load "range.scm")


(define (println s) (for-each (curry format #t "~a~%") s))


;; Consider
;;
;; ( (a-1)^n + (a+1)^n ) mod a^2 = r
;;
;; so
;;
;; (a-1)^n + (a+1)^n = q a^2 + r
;;
;; The r is one of a^2-1, a^2-2, ..., 1,
;;
;; so
;;
;; (a-1)^n + (a+1)^n = q a^2 + a^2 - f, where f is 1,2,...
;;
;; (a-1)^n + (a+1)^n = a^2(q+1) - f
;;
;; let Q=q+1:
;;
;; (a-1)^n + (a+1)^n = Q a^2 - f
;;
;; so
;;
;;     (a-1)^n + (a+1)^n + f
;; Q = ---------------------     (A) 
;;            a^2
;;
;; Q must be integer.
;;
;; f is fixed.
;;
;; n is 1,2,3...
;;
;; R(f,n) be remainder of A(f,n). If its 0, then f is what we're looking for.
;; If R(f,n)==R(f,1) then [1,n-1] is a complete period, all the rest R() will
;; be the same (DETERMINED VIA NON-SCIENTIFIC OSERVATION OF RESULTS).
;;
;; So result is either n or #f.
(define (good-f? a f)
  (let ((aa (* a a))
        (a--1 (1- a))
        (a++1 (1+ a)))

    (define (n->qrem a-1 a+1) (remainder (+ f a-1 a+1) aa))

    (let ((n1 (n->qrem a--1 a++1)))

      (let next ((n 1) (a-1 a--1) (a+1 a++1))
        (let ((qrem (n->qrem a-1 a+1)))
          (if (zero? qrem)
            n
            (if (and (> n 1) (= n n1))
              #f
              (next (1+ n) (* a-1 a--1) (* a+1 a++1)))))))))


;; Find maximum r for given a.
(define (max-rem a)
  (let next ((f 1))
    (let ((n (good-f? a f)))
      (if n
        (values a f n (- (* a a) f))
        (next (1+ f))))))


;; NON-SCIENTIFIC OSERVATION OF RESULTS showed that f=a for odd a, and
;; f=2a for even a.
(define (max-rem-magical a)
  (- (* a a) (if (odd? a) a (* a 2))))


(define (p120)
  (apply + (map max-rem-magical (range 3 1000))))


;; end of file
;; vim: sw=4 ts=4
