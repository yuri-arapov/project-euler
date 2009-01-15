;; 18 March 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=38
;;
;; Problem 38
;; 28 February 2003
;; 
;; Take the number 192 and multiply it by each of 1, 2, and 3:
;; 
;;     192 × 1 = 192
;;     192 × 2 = 384
;;     192 × 3 = 576
;; 
;; By concatenating each product we get the 1 to 9 pandigital, 192384576. We
;; will call 192384576 the concatenated product of 192 and (1,2,3)
;; 
;; The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4,
;; and 5, giving the pandigital, 918273645, which is the concatenated product
;; of 9 and (1,2,3,4,5).
;; 
;; What is the largest 1 to 9 pandigital 9-digit number that can be formed as
;; the concatenated product of an integer with (1,2, ... , n) where n > 1?
;;
;; Answer: 932718654


(load "print.scm")


(define (range from to)
  (if (> from to)
    '()
    (cons from (range (+ from 1) to))))


(define (number-of-digits n)
  (if (< n 10)
    1
    (+ 1 (number-of-digits (quotient n 10)))))


(define (number->digits n)
  (if (< n 10)
    (list n)
    (append (number->digits (quotient n 10)) (list (remainder n 10))))) ;; 123 -> (1 2 3)
;;    (cons (remainder n 10) (number->digits (quotient n 10))))) ;; 123 -> (3 2 1)


(define (pandigital? n)
  (let ((l (quick-sort (number->digits n)
                       (lambda (x y) (< x y)))))
    (equal? l '(1 2 3 4 5 6 7 8 9))))


(define (concatenate nn)
  (define (iter res nn)
    (if (null? nn) 
      res
      (let ((res-len (number-of-digits res))
            (n       (car nn)))
        (iter (+ res (* n (expt 10 res-len))) 
              (cdr nn)))))
  (let ((nn (reverse nn)))
    (if (null? nn)
      0
      (iter (car nn) 
            (cdr nn)))))


(define (concatenated-product i n)
  (concatenate 
    (map (lambda (x) (* x i)) 
         (range 1 n))))


(define (p38)
  (define (iter maxsofar i n)
    (let* ((iprod (concatenated-product i n))
           (len   (number-of-digits iprod)))
      (cond ((= i 1) 
             maxsofar)
            ((> len 9)
             (iter maxsofar (- i 1) 2))
            ((< len 9)
             (iter maxsofar i (+ n 1)))
            ((= len 9)
             (if (and (pandigital? iprod) (> iprod maxsofar))
               (begin
                 (println i "x" (range 1 n) " -> " iprod)
                 (iter iprod i (+ n 1)))
               (iter maxsofar i (+ n 1)))))))
  (iter 0 9999 2))


;; end of file

