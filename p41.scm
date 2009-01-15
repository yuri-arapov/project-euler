;; 20 March 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=41
;;
;; Problem 41
;; 11 April 2003
;;
;; We shall say that an n-digit number is pandigital if it makes use of all the
;; digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
;; also prime.
;;
;; What is the largest n-digit pandigital prime that exists?
;;
;; Answer: 7652413


(load "prime.scm")
(load "permutations.scm")
(load "print.scm")
(load "range.scm")


(define (digits->number digits)
;; (1 2 3) -> 123
;;
  (reduce (lambda (x res) (+ (* res 10) x)) 0 digits))


(define (fac n)
;; n!
;;
;; (fac 3) -> 6
;;
  (define (iter res n)
    (if (< n 1)
      res
      (iter (* res n) (- n 1))))
  (iter 1 n))


(define (find p? ls)
;; return either first list member that that makes predicate
;; p? be true or false if no such list item at all
;;
;; (find even? '(1 2 3)) -> 2
;; (find zero? '(1 2 3)) -> #f
;;
  (cond ((null? ls) #f)
        ((p? (car ls)) (car ls))
        (else (find p? (cdr ls)))))


(define (p41)
  (define (iter n)
    (println n "-pandigital, " (fac n) " candidates to test")
    (let* ((all-n-pandigital 
             (map digits->number 
                  (permutations (reverse (range 1 n)))))
                             ;;  ^^^^^^^
                             ;;  this brings greater numbers to be
                             ;;  in front of the list
                             ;;  (see permutations.scm for more details)
                             ;;
           (p (find prime? all-n-pandigital)))
      (if (number? p)
        p                 ;; stop when found because it's the maximum prime
        (iter (- n 1)))))
  (iter 7))
;; NOTE 
;;   It used to be (iter 9), but the very first forum's post
;;   (http://projecteuler.net/index.php?section=forum&id=41) gave a good idea
;;   to exclude 9- and 8- pandigitals from consideration:
;;   any 9-pandigital number is divisible by 3, because
;;
;;    1+2+3+4+5+6+7+8+9=45, and 45 is divided by 3 evenly.
;;
;;   Same thing about 8-pandigital numbers:
;;
;;    1+2+3+4+5+6+7+8=36 and 36 is divided by 3 evenly.
;;
;;   So first n-pandigital to try is 7.
;;
      

;; end of file
