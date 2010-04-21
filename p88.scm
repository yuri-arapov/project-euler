;; 2010-03-15
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=88
;;
;; Problem 88
;; 04 February 2005
;;
;; A natural number, N, that can be written as the sum and product of a given
;; set of at least two natural numbers, {a1, a2, ... , ak} is called a
;; product-sum number: N = a1 + a2 + ... + ak = a1 x a2 x ... x ak.
;;
;; For example, 6 = 1 + 2 + 3 = 1 x 2 x 3.
;;
;; For a given set of size, k, we shall call the smallest N with this property
;; a minimal product-sum number. The minimal product-sum numbers for sets of
;; size, k = 2, 3, 4, 5, and 6 are as follows.
;;
;; k=2:  4 = 2 x 2                 = 2 + 2
;; k=3:  6 = 1 x 2 x 3             = 1 + 2 + 3
;; k=4:  8 = 1 x 1 x 2 x 4         = 1 + 1 + 2 + 4
;; k=5:  8 = 1 x 1 x 2 x 2 x 2     = 1 + 1 + 2 + 2 + 2
;; k=6: 12 = 1 x 1 x 1 x 1 x 2 x 6 = 1 + 1 + 1 + 1 + 2 + 6
;;
;; Hence for 2 <= k <= 6, the sum of all the minimal product-sum numbers is
;; 4+6+8+12= 30; note that 8 is only counted once in the sum.
;;
;; In fact, as the complete set of minimal product-sum numbers for 2 <= k <= 12 is 
;; {4, 6, 8, 12, 15, 16}, the sum is 61.
;;
;; What is the sum of all the minimal product-sum numbers for 2 <= k <= 12000?
;;
;; Answer: ???


(load "uniq.scm")


(define (inc s res)
  (let loop ((head '())
             (tail s)
             (res res))
    (cond
      ((null? tail)   
       res)

      ((and (not (null? head))
            (= (car head) (car tail)))
       (if (= 1 (car tail))
         res
         (loop (cons (car tail) head) (cdr tail) res)))

      (else
        (loop (cons (car tail) head)
              (cdr tail)
              (cons (append (reverse (cons (1+ (car tail)) head)) (cdr tail)) res))))))


(define (f n)
  (let loop ((s (list (make-list n 1)))
             (n (1+ n)))
    (let* 
      ((x (fold inc '() s))
       (y (filter (lambda (i) (>= n (apply * i))) x))
       ;;(y x)
       (z (find (lambda (i) (= n (apply * i))) y)))
      (format #t "(len x) ~a, (len y) ~a\n" (length x) (length y))
      (if z
        (list (apply + z) z)
        (loop y (1+ n))))))


(define (mul s1 s2)
  (let ((x (cond 
             ((null? s1) s2)
             ((null? s2) s1)
             (else 
               (let loop ((x1 s1)
                          (x2 s2)
                          (res '()))
                 (cond 
                   ((null? x1) res)
                   ((null? x2) (loop (cdr x1) s2 res))
                   (else
                     (loop x1 
                           (cdr x2) 
                           (cons (* (car x1) (car x2)) res)))))))))
    (uniq (sort x <))))


;; end of file
;; vim: sw=4 ts=4
