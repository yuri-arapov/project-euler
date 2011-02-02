;; 2011-02-02
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=119
;;
;; Problem 119
;; 07 April 2006
;;
;; The number 512 is interesting because it is equal to the sum of its digits
;; raised to some power: 5 + 1 + 2 = 8, and 83 = 512. Another example of a
;; number with this property is 614656 = 284.
;;
;; We shall define an to be the nth term of this sequence and insist that a
;; number must contain at least two digits to have a sum.
;;
;; You are given that a2 = 512 and a10 = 614656.
;;
;; Find a30.
;;
;; Answer: 248155780267521


(load "range.scm")
(load "uniq.scm")


;; Turn number into list of digits.
;; Lower digits go first:
;; (number->digits 123) -> (3 2 1)
(define (number->digits n)
  (unfold
    zero?                               ;; stop when seed is 0
    (lambda (s) (remainder s 10))       ;; make next list element from the seed
    (lambda (s) (quotient s 10))        ;; make next seed 
    n))                                 ;; seed


;; Turn list of digits into number.
;; Counterpart of (number->digits).
(define (digits->number digits)
  (fold-right (lambda (d res) (+ d (* res 10))) 0 digits))


;; Turn number into sum of its digits.
(define (sum-of-digits n) (apply + (number->digits n)))


;; True if m is some power on n.
(define (ispower? m n)
  (let loop ((x n))
    (or (= x m)
        (and (< 1 x m) ;; x is not 1 and less than m
             (loop (* x n))))))


;; True if n is a power of sum of its digits:
;;                    p
;;   abc = (a + b + c)
(define (wierd-number? n)
  (and (> n 10) (ispower? n (sum-of-digits n))))


;; Make list of powers of n (starting from 2) that <= than limit.
(define (number->powers limit n)
  (unfold
    (curry < limit)     ;; stop when limit < seed
    identity            ;; list element = seed
    (curry * n)         ;; next seed = seed * n
    (* n n)))           ;; initial seed


;; Return nth-term of wierd (see above) numbers.
(define (p119-int max-digits-count nth-term)
  (let* ((max-digits-sum   (* 9 max-digits-count))
         (max-number       (digits->number (make-list max-digits-count 9)))
         (wierd-numbers    (filter
                             wierd-number?
                             (uniq
                               (sort
                                 (apply append
                                        (map (curry number->powers max-number)
                                             (range 2 max-digits-sum)))
                                 <)))))
    (and (>= (length wierd-numbers) nth-term)
         (list-ref wierd-numbers (1- nth-term)))))


(define (p119)
  (let loop ((max-digits 2))
    (or (p119-int max-digits 30) 
        (loop (* 2 max-digits)))))


;; end of file
;; vim: sw=4 ts=4
