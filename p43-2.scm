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


(load "range.scm")
(load "permutations.scm")
(load "print.scm")


(define (digits->number dd)
;; (1 2 3) -> 123
;;
  (reduce (lambda (x res) (+ (* res 10) x)) 0 dd))


(define (divisible? n d)
;; test if given number n (represented as a list of digits) is divisible by d
;;
;; example:
;;   (divisible? '(1 2 3) 3) -> #t
;;   (divisible? '(1 2 3) 2) -> #f
;;
  (zero? (remainder (digits->number n) d)))


(define (no-such-digit? n d)
;; test if digit d is not in n
;;
  (not (memq d n)))


(define (has-digit? n d)
  (not (no-such-digit? n d)))


(define (grow numbers divisor)
  (define (loop res numbers digit)
    (cond ((null? numbers) res)
          ;; all the list is done, return result

          ((> digit 9) (loop res (cdr numbers) 0)) 
          ;; 0..9 digits are done for given number,
          ;; work out next number

          ((has-digit? (car numbers) digit) (loop res numbers (+ digit 1)))
          ;; the number has this digit, try next digit

          (else
            (let* ((candidate (cons digit (car numbers)))
                   (c3        (list-head candidate 3))) ;; first three digits
              (if (divisible? c3 divisor)
                (loop (append res (list candidate)) numbers (+ digit 1))
                (loop         res                   numbers (+ digit 1)))))))
  (loop '() numbers 0))


(define (add-missing-digit n)
  (define (loop digit)
    (cond ((> digit 9) (error "add-missing-digit failed: all the digits already there"))
          ((no-such-digit? n digit) (cons digit n))
          (else (loop (+ digit 1)))))
  (loop 0))


(define (p43)

  (define (grow-loop res divisors)
    (if (null? divisors)
      res
      (grow-loop (grow res (car divisors)) (cdr divisors))))

  (let* ((ls-17 (filter (lambda (x) (divisible? x 17)) (permutations-n (range 0 9) 3)))
         (t1 (grow-loop ls-17 '(13 11 7 5 3 2)))
         (t2 (map add-missing-digit t1)))
    (println t2)
    (apply + (map digits->number t2))))


;; end of file
