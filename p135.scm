;; 2011-03-12
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=135
;;
;; Problem 135
;; 29 December 2006
;;
;; Given the positive integers, x, y, and z, are consecutive terms of an
;; arithmetic progression, the least value of the positive integer, n, for
;; which the equation, x^2 - y^2 - z^2 = n, has exactly two solutions is n = 27:
;;
;; 34^2 - 27^2 - 20^2 = 12^2 - 9^2 - 6^2 = 27
;;
;; It turns out that n = 1155 is the least value which has exactly ten
;; solutions.
;;
;; How many values of n less than one million have exactly ten distinct
;; solutions?
;;
;; Answer: 4989


;; maxima:
;;  (%i1) expand( (y+k)^2 - y^2 - (y-k)^2 );
;;                 2
;;  (%o1) 4 k y - y
(define (fy y k)        (- (* 4 k y) (* y y)))


(define (k n y)         (/ (+ (/ n y) y) 4))


(define (good-k? k y)   (and (integer? k) (< k y)))
(define (bingo? n y)    (good-k? (k n y) y))


(define (string->numbers s)
  (filter identity (map string->number (string-split s #\space))))


;; Reorder list of divisors and skip '1':
;;   (36 1 2 3 4 6 9 12 18) -> (36 18 12 9 6 4 3 2)
(define (reorder-divisors s)
  (cons (car s) (reverse (cddr s))))


(define (p135)
  (count
    (lambda (s) ; n and all its divisors 
      (= 10 (count (curry bingo? (car s)) s)))
    (time (read-file-with 
      "proper-divisors-1000000"
      (compose reorder-divisors string->numbers)))))



;; end of file
;; vim: sw=4 ts=4
