;; 13 March 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=35
;;
;; Problem 35
;; 17 January 2003
;;
;; The number, 197, is called a circular prime because all rotations of the
;; digits: 197, 971, and 719, are themselves prime.
;;
;; There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37,
;; 71, 73, 79, and 97.
;;
;; How many circular primes are there below one million?
;;
;; Answer: 55


(load "print.scm")
(load "prime.scm")


(define (number-of-digits n)
  (if (< n 10)
    1
    (+ 1 (number-of-digits (quotient n 10)))))


(define (rotate-right n multiplier)
  (+ (* (remainder n 10) multiplier)
     (quotient n 10)))


(define (circular-prime? n)
  (define (test m n rotations-left)
    ;; (println n " " r)
    (and (prime? n) 
         (or (zero? rotations-left)
             (test m 
                   (rotate-right n m) 
                   (- rotations-left 1)))))

  (let* ((digits     (number-of-digits n))
         (multiplier (expt 10 (- digits 1))))
    (test multiplier n (- digits 1))))


(define (filtered-accum filter? op init from to)
  (define (iter res n)
    (if (> n to)
      res
      (if (filter? n)
        (iter (op res n) (+ n 1))
        (iter res (+ n 1)))))

  (iter init from))


(define (p35)
  (length (filtered-accum circular-prime? 
                          (lambda (x y)
                            (println y)
                            (append x (list y)))
                          '()
                          2          ;; first prime
                          999999)))  ;; "... below one million"


;; end of file
