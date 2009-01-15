;; 23 March 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=43
;;
;; Problem 43
;; 09 May 2003
;; 
;; The number, 1406357289, is a 0 to 9 pandigital number because it is made up
;; of each of the digits 0 to 9 in some order, but it also has a rather
;; interesting sub-string divisibility property.
;; 
;; Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we
;; note the following:
;; 
;;     * d2d3d4=406 is divisible by 2
;;     * d3d4d5=063 is divisible by 3
;;     * d4d5d6=635 is divisible by 5
;;     * d5d6d7=357 is divisible by 7
;;     * d6d7d8=572 is divisible by 11
;;     * d7d8d9=728 is divisible by 13
;;     * d8d9d10=289 is divisible by 17
;; 
;; Find the sum of all 0 to 9 pandigital numbers with this property.
;;
;; Answer: 16695334890
;;
;; NOTE: see p43-2.scm (muuuuch faster)


(load "range.scm")
(load "permutations.scm")
(load "print.scm")


(define (digits->number dd)
;; (1 2 3) -> 123
;;
  (reduce (lambda (x res) (+ (* res 10) x)) 0 dd))


(define (divisible? n d)
;; test if given number n is divisible by d
;;
;; example:
;;   (divisible? '(1 2 3) 3) -> #t
;;   (divisible? '(1 2 3) 2) -> #f
;;
  (zero? (remainder (digits->number n) d)))


(define (last-digit n)
  (car (last-pair n)))


(define (last-two-digits n)
  (list-tail n (- (length n) 2)))


(define (first-two-digits n)
  (list-head n 2))


(define (no-such-digit? n d)
  (not (memq d n)))


(define (chain-numbers? a b)
  (equal? (last-two-digits a) (first-two-digits b)))


(define (join-numbers a b)
  (list (append a (last-pair b))))


(define (combine lefts rights)
  (define (loop res ll rr)
    (cond ((null? ll) res)
          ((null? rr) (loop res (cdr ll) rights))
          (else
            (let ((l (car ll))
                  (r (car rr)))
              (if (and (chain-numbers? l r)
                       (no-such-digit? l (last-digit r)))
                (loop (append res (join-numbers l r)) ll (cdr rr))
                (loop res ll (cdr rr)))))))
  (loop '() lefts rights))


(define three-digits-numbers (permutations-n (range 0 9) 3))


(define four-digits-even-numbers (filter (lambda (x) (even? (cadddr x))) 
                                         (permutations-n (range 0 9) 4)))


(define (p43)

  (define (divisors-loop res divisors)
    (if (null? divisors)
      res
      (divisors-loop (combine res 
                              (filter (lambda (x) (divisible? x (car divisors))) 
                                      three-digits-numbers))
                     (cdr divisors))))

  (apply + (map digits->number (divisors-loop four-digits-even-numbers '(3 5 7 11 13 17))))

  )


;; end of file
