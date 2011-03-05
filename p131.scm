;; 2011-03-05
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=131
;;
;; Problem 131
;; 10 November 2006
;;
;; There are some prime values, p, for which there exists a positive integer,
;; n, such that the expression n3 + n2p is a perfect cube.
;;
;; For example, when p = 19, 83 + 8219 = 123.
;;
;; What is perhaps most surprising is that for each prime with this property
;; the value of n is unique, and there are only four such primes below
;; one-hundred.
;;
;; How many primes below one million have this remarkable property?
;;
;; Answer: 173
;;
;; NOTE. Cowardly stolen here: 
;;       http://wiki.san-ss.com.ar/project-euler-problem-131


(load "miller-rabin-primality-test.scm")


;; Cube of the x.
(define (cube x) (* x x x))


;; Check if x below limit: return either x or false (if test not passed).
(define (check-limit limit x) (and (< x limit) x))


;; Problem 131, limit provided.
(define (p131-ex limit)
  (length
    (filter 
      prime?
      (generate-list ; see ~/scheme/scheme-utils.scm
        (lambda (a) (check-limit limit (- (cube a) (cube (1- a)))))
        1+
        2))))


;; Problem 131.
(define (p131)
  (p131-ex 1000000))


;; end of file
;; vim: sw=4 ts=4
