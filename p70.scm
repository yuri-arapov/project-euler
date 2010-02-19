;; 2010-02-17
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=70
;;
;; Problem 70
;; 21 May 2004
;;
;; Euler's Totient function, φ(n) [sometimes called the phi function], is used
;; to determine the number of positive numbers less than or equal to n which
;; are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all
;; less than nine and relatively prime to nine, φ(9)=6.
;; The number 1 is considered to be relatively prime to every positive number, so φ(1)=1.
;;
;; Interestingly, φ(87109)=79180, and it can be seen that 87109 is a
;; permutation of 79180.
;;
;; Find the value of n, 1  n  107, for which φ(n) is a permutation of n and the
;; ratio n/φ(n) produces a minimum.
;;
;; Answer: 8319823
;;


(load "miller-rabin-primality-test.scm")
(load "range.scm")


;; Turn number into list of digits, lower digits first.
;;
(define (number->digits n)
  (if (zero? (quotient n 10))
    (list n)
    (cons (remainder n 10) (number->digits (quotient n 10)))))


;; Turn list of digits (lower digits first) into a number.
;; FOR DEBUGGING
;;
(define (digits->number ds)
  (fold-right (lambda (x res) (+ x (* res 10))) 0 ds))


;; Turn number into list of sorted digits.
;;
(define (number->norm n)
  (sort (number->digits n) >))


;; Make sure that number b is a permutation of digits of number a.
;;
(define (permuted-numbers? a b)
  (equal? (number->norm a) (number->norm b)))


;; Turn number into list of primes that factorize given number.
;; FOR DEBUGGING
;;
(define (number->factors n)
  (let loop ((n n)
             (p 2)
             (res '()))
    (cond ((= 1 n)
           (reverse res))
          ((prime? n)
           (reverse (cons n res)))
          ((zero? (remainder n p))
           (loop (quotient n p) p (cons p res)))
          (else
            (loop n (if (= p 2) 3 (+ p 2)) res)))))


;; Remove duplicated consecutive elements from the list.
;; FOR DEBUGGING
;;
(define (uniq s)
  (if (null? s)
    s
    (let loop ((s (cdr s))
               (res (list (car s))))
      (cond ((null? s)
             (reverse res))
            ((= (car s) (car res))
             (loop (cdr s) res))
            (else
              (loop (cdr s) (cons (car s) res)))))))


;; Phi (Euler) function.
;;
(define (phi n) 
  (* n 
     (apply * 
            (map (lambda (p) (- 1 (/ 1 p))) 
                 (uniq (number->factors n))))))


;; Solve problem 70.
;;
(define (p70)
  (define (iter ps1 ps2 n f r)
    (cond ((null? ps1)
           (list n f))

          ((null? ps2)
           (iter (cdr ps1) (cdr ps1) n f r)) ;; note that first and second
                                             ;; arguments are the same

          ((= (car ps1) (car ps2))
           (iter ps1 (cdr ps1) n f r))

          (else
            (let* ((p1  (car ps1))
                   (p2  (car ps2))
                   (nn  (* p1 p2))

                   (ff  (* (- p1 1) (- p2 1))) ;; Euler function when n is a 
                                               ;; product of two primes
                   (rr  (/ nn ff)))

              (if (and (<= 1000000 nn 10000000)
                       (< rr r)
                       (permuted-numbers? nn ff))
                (begin
                  ;; better result found
                  (format #t "~a ~a -> ~a ~a -> ~a\n" p1 p2 nn ff (* rr 1.0))
                  (iter ps1 (cdr ps2) nn ff rr))
                (iter ps1 (cdr ps2) n f r))))))

  (let ((p (filter prime? (range 1000 9999)))) ;; four-digit primes
    (iter p (cdr p) 0 0 2)))


;; end of file
