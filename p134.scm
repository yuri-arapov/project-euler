;; 2011-03-09
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=134
;;
;; Problem 134
;; 15 December 2006
;;
;; Consider the consecutive primes p1 = 19 and p2 = 23. It can be verified that
;; 1219 is the smallest number such that the last digits are formed by p1
;; whilst also being divisible by p2.
;;
;; In fact, with the exception of p1 = 3 and p2 = 5, for every pair of
;; consecutive primes, p2>p1, there exist values of n for which the last
;; digits are formed by p1 and n is divisible by p2. Let S be the smallest of
;; these values of n.
;;
;; Find sum(S) for every pair of consecutive primes with 5<=p1<=1000000.
;;
;; Answer: 18613426663617118


;; Last decimal digit of the number.
(define (last-digit n)
  (remainder n 10))


;; Number with last decimal digit dropped.
(define (drop-last-digit n)
  (quotient n 10))


;; Number with given digit appended at the right.
(define (add-digit-right digit n)
  (+ digit (* n 10)))


;; Trun number into list of digits, LOWER digits go first.
(define (number->digits n)
  (unfold zero? last-digit drop-last-digit n))


;; Turn list of digits into number, HIGHER digits go first.
(define (digits->number digits)
  (fold add-digit-right 0 digits))


;; Find digit m so that last digit of number
;;   m*src-digit + add-digit 
;; is
;;   dst-digit.
(define (find-mul-digit src-digit add-digit dst-digit)
  (find 
    (lambda (d) (= dst-digit (last-digit (+ (* src-digit d) add-digit))))
    '(0 1 2 3 4 5 6 7 8 9)))


;; Compute S for consecutive primes p1 p2 (p1 < p2).
(define (S p1 p2)
  (let ((src-digit (last-digit p2)))
    (let loop ((dst-digits (number->digits p1))
               (adder      0)
               (mul-digits '()))
      (if (null? dst-digits)
        (* p2 (digits->number mul-digits))
        (let ((m (find-mul-digit src-digit (last-digit adder) (car dst-digits))))
          (if (not m) #f
            (loop (cdr dst-digits)
                  (drop-last-digit (+ adder (* m p2)))
                  (cons m mul-digits))))))))


;; Problem 134, limit provided.
(define (p134-ex limit)
  (with-return (return)
    (pair-fold 
      (lambda (primes ss)
        (if (> (car primes) limit) (return ss)
          (+ ss (S (car primes) (cadr primes)))))
      0
      (drop (read-file-with "first-1000000-primes" string->number) 
            2)))) ; skip 2 and 3


;; Problem 134.
(define (p134)
  (p134-ex 1000000))


;; end of file
;; vim: sw=4 ts=4
