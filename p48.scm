;; 23 March 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=48
;; 
;; Problem 48
;; 18 July 2003
;;
;; The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
;;
;; Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
;;
;; Answer: 9110846700
;;


(load "range.scm")


(define (p48-n n)
  (apply + (map (lambda (x) (remainder (expt x x) 10000000000))
                (range 1 n))))


(define (p48)
  (remainder (p48-n 1000) 10000000000))


;; end of file
