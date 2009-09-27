;; 2009-09-27
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; Problem 74
;; 16 July 2004
;;
;; The number 145 is well known for the property that the sum of the factorial
;; of its digits is equal to 145:
;;
;; 1! + 4! + 5! = 1 + 24 + 120 = 145
;;
;; Perhaps less well known is 169, in that it produces the longest chain of
;; numbers that link back to 169; it turns out that there are only three such
;; loops that exist:
;;
;; 169 -> 363601 -> 1454 -> 169
;; 871 -> 45361 -> 871
;; 872 -> 45362 -> 872
;;
;; It is not difficult to prove that EVERY starting number will eventually get
;; stuck in a loop. For example,
;;
;; 69 -> 363600 -> 1454 -> 169 -> 363601 (-> 1454)
;; 78 -> 45360 -> 871 -> 45361 (-> 871)
;; 540 -> 145 (-> 145)
;;
;; Starting with 69 produces a chain of five non-repeating terms, but the
;; longest non-repeating chain with a starting number below one million is sixty
;; terms.
;;
;; How many chains, with a starting number below one million, contain exactly
;; sixty non-repeating terms?
;;
;; Answer: 402
;;
;; Running time 3 minutes.


(load "fac.scm")
(load "permutations.scm")
(load "range.scm")


;;
;; Factorials of each digit as a vector.
;;
(define *fac-of-digits* (list->vector (map fac '(0 1 2 3 4 5 6 7 8 9))))


;;
;; Return factorial of given digit.
;;
(define (digit-fac digit) (vector-ref *fac-of-digits* digit))


;;
;; Turn number into list of decimal digits, higher digits first:
;; 123 -> '(1 2 3)
;;
(define (number->digits n)
  (let loop ((ls '())
             (n n))
    (if (< n 10)
      (cons n ls)
      (loop (cons (remainder n 10) ls) (quotient n 10)))))


;;
;; Turn list of decimal digits (higher digits first) into number:
;; '(1 2 3) -> 123
;;
(define (digits->number digits)
  (fold (lambda (n res) (+ (* res 10) n)) 0 digits))


;;
;; Compute next number as a sum of factorials of number's digits.
;;
(define (next-number n) (apply + (map digit-fac (number->digits n))))


;;
;; Return list of numbers that would give the same (next-number n) value as 
;; given one.
;;
(define (number->same-next-numbers num)
  (map
    digits->number
    (filter
      (lambda (l) (not (zero? (car l))))
      (permutations (number->digits num)))))


;;
;; Vector containing length of non-repeating chain for each number
;; from 0 to 9999999.
;;
;; Note: this value is an upper bound determined so that (next-number 999999) 
;;       and its permutations would fit in:
;;       (apply max (number->same-next-numbers (next-number 999999)))
(define *chain-len* (make-vector 9999999 #f))


;;
;; Return length of non-repeating chain for given number.
;;
(define (get-chain-len number) (vector-ref *chain-len* number))


;;
;; Set length of non-repeating chain for given number.
;;
(define (set-chain-len! number len) (vector-set! *chain-len* number len))


;;
;; Set length of non-repeating chain for given number and all its permutations.
;;
(define (set-chain-len-perm! number len)
  (for-each (lambda (n) (set-chain-len! n len))
            (number->same-next-numbers number)))


;;
;; Chain lengths for some numbers.
;;
(define *known-len*
  '((1      1)
    (2      1)
    (145    1)
   
    (40585  1) ;; This one is a tricky one, it's not listed in
               ;; problem definition, so program was hanging 
               ;; in infinite loop trying to resolve it.
   
    (169    3)
    (363601 3)
    (1454   3)
    (871    2)
    (45361  2)
    (872    2)
    (45362  2)))


;; 
;; Update known chain lengths.
;;
(for-each (lambda (p) (set-chain-len! (car p) (cadr p))) *known-len*)


;;
;; Fill in chain lengths of numbers in the list.
;;
(define (populate-chain-len ls len)
  (if (null? ls)
    (- len 1)
    (begin
      (if (not (get-chain-len (car ls)))
        (set-chain-len-perm! (car ls) len))
      (populate-chain-len (cdr ls) (+ 1 len)))))


;;
;; Return length of non-repeating numbers chain that starts with
;; given number.
;;
(define (number->chain-len number)
  (let loop ((ls (list number)))
    (let ((n (car ls)))
      (if (get-chain-len n)
        (populate-chain-len (cdr ls) (+ 1 (get-chain-len n)))
        (loop (cons (next-number n) ls))))))


;;
;; Progress indicator.
;;
(define (show-progress n)
  (if (zero? (remainder n 10000))
    (format #t "~a~%" n)))


;;
;; Problem 74.
;;
(define (p74)
  (length 
    (filter 
      (lambda (n)
        (show-progress n)
        (= 60 (number->chain-len n)))
      (range 1 999999))))


;; end of file
