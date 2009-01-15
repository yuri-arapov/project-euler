;; 18 March 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=40
;;
;; Problem 40
;; 28 March 2003
;;
;; An irrational decimal fraction is created by concatenating the
;; positive integers:
;;
;; 0.123456789101112131415161718192021...
;;              ^
;;
;; It can be seen that the 12th digit of the fractional part is 1.
;;
;; If dn represents the nth digit of the fractional part, find the
;; value of the following expression.
;;
;; d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
;;
;; Answer: 210


(define (digits-in-block digits-per-number)
;; return number of digits in the block of numbers.
;;
;; examle:
;;   let digits-per-number be 3, i.e. the block of numbers is
;;
;;     100101102103...999
;;
;;   there are 900 3-digit numbers in the block.
;;   so the result is 2700.
;;
  (* 9 (expt 10 (- digits-per-number 1)) digits-per-number))


(define (number->digits n)
;; return list of digits (left-to-right) given number consists of.
;;
;; example:
;;
;;   (number->digits 123) -> (1 2 3)
;;
  (if (< n 10)
    (list n)
    (append (number->digits (quotient n 10)) 
            (list (remainder n 10)))))


(define (get-nth-digit-from-block
          digits-per-number ;; amount of digits in each block's number
          nth-digit)        ;; origin 0

  (let* ((index (quotient nth-digit digits-per-number))       ;; index of number in the block 
                                                              ;; nth digit belongs to,
                                                              ;; origin 0

         (number (+ (expt 10 (- digits-per-number 1)) index)) ;; the number nth digit belongs to

         (x (remainder nth-digit digits-per-number)))         ;; index of digit (left-to-right)
                                                              ;; in the number, origin 0
    (list-ref (number->digits number) x)))


(define (get-nth-digit n) ;; origin 1, i.e. first digit is 1st, NOT 0st
  (define (iter digits-per-number n)
    (let ((max-digit (digits-in-block digits-per-number)))
      (if (< n max-digit)
        (get-nth-digit-from-block digits-per-number n)
        (iter (+ digits-per-number 1) (- n max-digit)))))
  (iter 1 (- n 1))) ;; switch to origin 0 to simplify the math


(define (p40)
  (apply * (map get-nth-digit '(1 10 100 1000 10000 100000 1000000))))


;; end of file
