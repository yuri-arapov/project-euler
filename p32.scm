;; 18 March 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=32
;;
;; Problem 32
;;
;; 06 December 2002
;; 
;; The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
;; multiplicand, multiplier, and product is 1 through 9 pandigital.
;; 
;; Find the sum of all products whose multiplicand/multiplier/product identity
;; can be written as a 1 through 9 pandigital.
;;
;; HINT: Some products can be obtained in more than one way so be sure to only
;; include it once in your sum.
;;
;; Answer: 45228


(define (number->digits n)
  (if (< n 10)
    (list n)
    (cons (remainder n 10) (number->digits (quotient n 10)))))


(define (numbers->digits a . z)
;;   (define (iter ls nn)
;;     (if (null? nn)
;;       ls
;;       (iter (append ls (number->digits (car nn))) (cdr nn))))
;;   (iter (number->digits a) z))
  (fold (lambda (n res) (append res (number->digits n))) 
        (number->digits a) 
        z))


(define (pandigital? m1 m2 product)
  (equal? '(1 2 3 4 5 6 7 8 9) 
          (quick-sort (numbers->digits m1 m2 product) 
                      (lambda (x y) (< x y)))))


;; (define (remove-repeats ls)
;;   (define (iter res src)
;;     (if (null? src)
;;       res
;;       (if (not (memq (car src) (cdr src)))
;;         (iter (append res (list (car src))) (cdr src))
;;         (iter res (cdr src)))))
;;   (iter '() ls))


(define (remove-repeats ls)
  (if (null? ls)
    '()
    (if (not (memq (car ls) (cdr ls)))
      (cons (car ls) (remove-repeats (cdr ls)))
      (remove-repeats (cdr ls)))))


(define (p32-run m1-min m1-max m2-min m2-max)
  (define (collect reslist m1 m2)
    (cond ((> m2 m2-max)
           (collect reslist (+ m1 1) m1-min))
          ((> m1 m1-max)
           reslist)
          (else
            (let ((p (* m1 m2)))
              (if (pandigital? m1 m2 p)
                (collect (append reslist (list p)) m1 (+ m2 1))
                (collect reslist m1 (+ m2 1)))))))
  (collect '() m1-min m2-min))


(define (p32)
  (apply + (remove-repeats (append (p32-run 1 9  1000 9999)
                                   (p32-run 10 99  100 999)))))

 
;; end of file
