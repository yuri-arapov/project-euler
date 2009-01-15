;; 13 March 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=37
;;
;; Problem 34
;; 03 January 2003
;;
;; 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
;;
;; Find the sum of all numbers which are equal to the sum of the factorial of
;; their digits.
;;
;; Note: as 1! = 1 and 2! = 2 are not sums they are not included.
;;
;; Answer: 40730


(load "print.scm")
(load "number-digits.scm")
(load "fac.scm")


(define factorials (map fac (list 0 1 2 3 4 5 6 7 8 9)))
;; list of 0! 1! ... 9!
;; (in order not to compute them every time)
;;


(define (digit->factorial d) (list-ref factorials d))
;; return factorial of given digit d [0..9]
;;


(define limit (* 7 (digit->factorial 9)))
;; this is the upper limit of the numbers we test
;; since 
;;   
;;   7 * 9! < 9999999


(define (p34)
  (define (sum-digits-factorials n)
    (apply + (map digit->factorial (number->digits n))))

  (define (match? n)
    (= (sum-digits-factorials n) n))

  (define (collect ls n)
    (cond ((> n limit) ls)
          ((match? n) 
           (println n)
           (collect (append ls (list n)) (+ n 1)))
          (else (collect ls (+ n 1)))))

  (apply + (collect '() 3)))


;; end of file
