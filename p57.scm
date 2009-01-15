;; 04 Apr. 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=57
;;
;; Problem 57
;; 21 November 2003
;;
;; It is possible to show that the square root of two can be expressed as an
;; infinite continued fraction.
;;  ___
;; v 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
;;
;; By expanding this for the first four iterations, we get:
;;
;; 1 + 1/2 = 3/2 = 1.5
;; 1 + 1/(2 + 1/2) = 7/5 = 1.4
;; 1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
;; 1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
;;
;; The next three expansions are 99/70, 239/169, and 577/408, but the eighth
;; expansion, 1393/985, is the first example where the number of digits in the
;; numerator exceeds the number of digits in the denominator.
;;
;; In the first one-thousand expansions, how many fractions contain a numerator
;; with more digits than denominator?
;;
;; Answer: 153
;;
;; FIXME: bruteforce


(load "range.scm")


;; (define (digits n)
;; ;; (+ 1 (inexact->exact (floor (/ (log n) (log 10))))))
;;   (+ 1 (inexact->exact (floor (log10 n)))))
;;
;; this version fails on big (300+ digit) numbers
;;


(define (digits n)
  (let loop ((n n)
             (r 1))
    (if (< n 10)
      r
      (loop (quotient n 10) (+ r 1)))))


(define (cont-fract-sqrt-2 n)
  (define (iter n divisor)
    (if (= n 1)
      divisor
      (iter (- n 1) (/ 1 (+ 2 divisor)))))
  (+ 1 (iter n (/ 1 2))))


;; (define (p57)
;;   (let* ((cf (map cont-fract-sqrt-2 (range 1 1000)))
;;          (x (filter (lambda (n) (> (digits (numerator n)) (digits (denominator n)))) cf)))
;;     (length x)))


(define (p57)
;; this version is much faster than the one above because
;; it does not recompute all the divisor all over again.
;;

  (define (match? f)
    (> (digits (numerator f)) (digits (denominator f))))

  (let loop ((divisor (/ 1 2))
             (iter 1)
             (r 0))
    (if (> iter 1000)
      r
      (loop (/ 1 (+ 2 divisor))           ;; next divisor of continued fraction.
            (+ iter 1)                    ;; increase iteration number.
            (if (match? (+ 1 divisor))    ;; compute approximation of (sqrt 2)
              (+ r 1)                     ;;   and test it.
              r)))))


;; end of file
