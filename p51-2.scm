;; 2009-12-20
;;
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=51
;;
;; Problem 51
;; 29 August 2003
;;
;; By replacing the 1st digit of *3, it turns out that six of the nine
;; possible values: 13, 23, 43, 53, 73, and 83, are all prime.
;;
;; By replacing the 3rd and 4th digits of 56**3 with the same digit, this
;; 5-digit number is the first example having seven primes among the ten
;; generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663,
;; 56773, and 56993. Consequently 56003, being the first member of this family,
;; is the smallest prime with this property.
;;
;; Find the smallest prime which, by replacing part of the number (not
;; necessarily adjacent digits) with the same digit, is part of an eight prime
;; value family.
;;
;; Answer: 121313
;;
;; Note:
;;   Initially there was a '0' digit in (digit-indices ...) call.
;;   I changed it to '1' when '0' didn't provide result.
;;   Actually it could be any digit.


(load "png.scm")


;; Return true if argument is not null.  Beautifier.
;;
(define (not-null? x) (not (null? x)))


;; Turn number into list of digits, lower digits first.
;; Counterpart for digits->number.
;;
(define (number->digits n)
  (if (< n 10)
    (cons n '())
    (cons (remainder n 10) (number->digits (quotient n 10)))))


;; Turn list of digits (lower digits first) into number.
;; Counterpart of number->digits.
;;
(define (digits->number digits)
  (fold-right (lambda (n res) (+ n (* 10 res))) 0 digits))


;; Global prime numbers generator.
;;
(define *png* (make-primes-generator))


;; Test number for being a prime.
;;
(define (prime? n) (*png* 'prime? n))


;; Return list of indices of given digit d in the given list of digits.
;; Example:
;;   (digit-indices '(1 2 3 4 1) 1) -> (0 4)
;;
(define (digit-indices digits d)
  (let loop ((res '())
             (pos 0)
             (ls  digits))
    (cond ((null? ls)
           (reverse res))
          ((= (car ls) d)
           (loop (cons pos res) (+ 1 pos) (cdr ls)))
          (else
            (loop res (+ 1 pos) (cdr ls))))))


;; Replace all the digits in 'digits' list with given 'new-digit'
;; in every position defined in 'indices' list.
;;
;; Example:
;;   (replace-digit '(1 2 1 3 1 4) '(0 2 4) 2) -> (2 2 2 3 2 4)
;;
(define (replace-digit digits indices new-digit)
  (let loop ((ls  indices)
             (dd  digits)
             (pos 0)
             (res '()))
    (cond ((null? dd)
           (reverse res))
          ((and (not-null? ls) (= pos (car ls)))
           (loop (cdr ls) (cdr dd) (+ 1 pos) (cons new-digit res)))
          (else
            (loop ls (cdr dd) (+ 1 pos) (cons (car dd) res))))))


;; Parameters:
;;   digits -- number of digits of prime numbers.
;;   len    -- number of digits to be changed in a prime.
;;   family -- number of the primes in the family.
;;
;; Example:
;;   (p51 6 3 8)
;;
(define (p51-internal digits len family)
  (let ((from (integer-expt 10 (- digits 1)))
        (to   (integer-expt 10 digits))) ;; excluding
    (let loop ((n from))
      (cond ((= n to)
             #f)
            ((not (prime? n))
             (loop (+ 1 n)))
            (else
              (let* ((digits (number->digits n))        ;; digits of the number, lower first
                     (ones   (digit-indices digits 1))) ;; indices of digit 1
                (if (not (= len (length ones)))
                  (loop (+ 1 n))
                  (begin
                    (format #t "~a: ~a~%" n ones)
                    (let* ((x (map
                                (lambda (d)
                                  (digits->number (replace-digit digits ones d)))
                                '(0 1 2 3 4 5 6 7 8 9)))
                           (y (filter
                                 (lambda (n) (and (>= n from) (prime? n)))
                                 x)))
                      (if (= family (length y))
                        y
                        (loop (+ 1 n))))))))))))


;; Solve problem 51.
;; For each 6-digit prime
;; Get list of indicies of digit 1 in the number
;; Replace digit 1 with (0 1 2 3 4 5 6 7 8 9)
;; Test number for being prime
;; Count primes
;; Stop if number of primes is 8
;;
(define (p51)
  (p51-internal 6 3 8))


;; end of file
