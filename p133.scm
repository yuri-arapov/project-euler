;; 2011-03-08
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=133
;;
;; Problem 133
;; 01 December 2006
;;
;; A number consisting entirely of ones is called a repunit. We shall define
;; R(k) to be a repunit of length k; for example, R(6) = 111111.
;;
;; Let us consider repunits of the form R(10^n).
;;
;; Although R(10), R(100), or R(1000) are not divisible by 17, R(10000) is
;; divisible by 17. Yet there is no value of n for which R(10^n) will divide by
;; 19. In fact, it is remarkable that 11, 17, 41, and 73 are the only four
;; primes below one-hundred that can be a factor of R(10^n).
;;
;; Find the sum of all the primes below one-hundred thousand that will never be
;; a factor of R(10^n).
;;
;; Answer: 453647705


(load "scheme-utils.scm")   ; mod-pow


(define ten^30 (expt 10 30))    ; large enough power of ten


(define (factor? p)
  (= 1 (mod-pow 10 ten^30 p)))


(define (p133-ex limit)
  (apply + 3
    (filter 
      (compose not factor?)
      (unfold
        (lambda (s) (> (car s) limit))  ; stop when primes > than given limit
        (lambda (s) (car s))            ; prime
        (lambda (s) (cdr s))            ; next prime
        (read-file-with "primes-1000000" string->number)))))


(define (p133)
  (p133-ex 100000))


;; end of file
;; vim: sw=4 ts=4
