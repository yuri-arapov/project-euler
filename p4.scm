;; 2010-11-24
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; Problem 4
;; http://projecteuler.net/index.php?section=problems&id=4
;; 
;; A palindromic number reads the same both ways. The largest
;; palindrome made from the product of two 2-digit numbers is
;; 9009 = 91 x 99.
;; 
;; Find the largest palindrome made from the product of two
;; 3-digit numbers.
;; 
;; Answer: 906609
;; 


(define (number->digits n)
  (let loop ((n n)
             (res '()))
    (if (zero? n)
      res
      (loop (quotient n 10) (cons (remainder n 10) res)))))


(define (palindrome? n)
  (let* ((digits   (number->digits n))
         (half-len (quotient (length digits) 2))
         (l        (take          digits  half-len))
         (r        (take (reverse digits) half-len)))
    (equal? l r)))


(define (p4)
  (let loop ((n1 999)
             (n2 999)
             (res 0))
    (cond ((< n1 100)
           res)
          ((< n2 100)
           (loop (- n1 1) (- n1 1) res))
          (else
            (let ((nn (* n1 n2)))
              (loop n1 
                    (1- n2)
                    (if (and (palindrome? nn) (> nn res))
                      nn
                      res)))))))

;; end of file
;; vim: ts=4 sw=4 et
