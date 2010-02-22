;; 2010-02-21
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=72
;;
;; Problem 72
;; 18 June 2004
;;
;; Consider the fraction, n/d, where n and d are positive integers. If nd and
;; HCF(n,d)=1, it is called a reduced proper fraction.
;;
;; If we list the set of reduced proper fractions for d <= 8 in ascending order
;; of size, we get:
;;
;; 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3,
;; 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
;;
;; It can be seen that there are 21 elements in this set.
;;
;; How many elements would be contained in the set of reduced proper fractions
;; for d <= 1,000,000?
;;
;; Answer: ???
;;


(load "range.scm")
(load "uniq.scm")


(define (print s)
  (for-each (lambda (x) (format #t "~a\n" x)) s))


(define (n-f->phi n f)
  (* n (apply * (map (lambda (p) (- 1 (/ 1 p))) (uniq f)))))


(define (reduce n p)
  (if (zero? (remainder n p))
    (reduce (quotient n p) p)
    n))


(define (jump s n)
  (cond ((null? s)
         #f)
        ((zero? n)
         s)
        (else
          (jump (cdr s) (- n 1)))))


(define (factorize s)

  (define (update p ns fs)
    (cond ((not ns)
           #f)
          (else
            (set-car! ns (reduce (car ns) p))
            (set-car! fs (cons p (car fs)))
            (update p (jump ns p) (jump fs p)))))

  (let ((res (map (lambda (x) '()) s)))
    (let loop ((ns s)
               (fs res))
      (cond ((null? ns)
             res)
            ((= 1 (car ns))
             (loop (cdr ns) (cdr fs)))
            (else
              (format #t "~a\n" (car ns))
              (update (car ns) ns fs)
              (loop (cdr ns) (cdr fs)))))))


;; end of file
