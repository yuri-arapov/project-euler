;; We can easily verify that none of the entries in the first seven rows of
;; Pascal's triangle are divisible by 7:
;; 
;; 1
;; 1 1
;; 1 2  1
;; 1 3  3  1
;; 1 4  6  4  1
;; 1 5 10 10  5 1
;; 1 6 15 20 15 6 1
;; However, if we check the first one hundred rows, we will find that only 2361 of
;; the 5050 entries are not divisible by 7.
;; 
;; Find the number of entries which are not divisible by 7 in the first one
;; billion (109) rows of Pascal's triangle.
;; 
;; 
;; Answer: 2129970655314432
;;
;; NOTE: bruteforce.

(load "fac.scm")



(define (increment x)
  (if (null? x) '(1)
    (let ((n (car x)))
      (if (< n 6) (cons (1+ n) (cdr x))
        (cons 0 (increment (cdr x)))))))


(define (base7 n)
  (if (zero? n) '(0)
    (let loop ((n n) (res '()))
      (if (zero? n) res
        (loop (quotient n 7) (cons (remainder n 7) res))))))


(define (not-div7 n) (positive? (remainder n 7)))


(define (c n k)
  (/ (fac n) (* (fac k) (fac (- n k)))))


(define (p-row n)
  (map (lambda (i) (c n i)) (iota (+ 1 n))))


(define (p148-limit limit)
  (let loop ((n 1) (res 1))
    (if (= n limit) res
      (let ((q (quotient n 7))
            (r (remainder n 7)))
        (loop (1+ n) (+ res (* (+ 1 r) (+ 1 q))))))))


(define (p148-limit2 limit)
  (let loop ((n 0) (s '(0)) (res 0))
    (if (= n limit) (begin (format #t "\n") res)
      (loop (begin (if (zero? (remainder n 100000)) (format #t "~:d ~a\r" n res)) (1+ n))
            (increment s)
            (+ res (apply * (map (curry + 1) s)))))))


;; end of file
