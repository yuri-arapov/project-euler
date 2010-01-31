;; 2009-12-29
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=60
;;
;; Problem 60
;; 02 January 2004
;;
;; The primes 3, 7, 109, and 673, are quite remarkable. By taking any two
;; primes and concatenating them in any order the result will always be prime.
;; For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of
;; these four primes, 792, represents the lowest sum for a set of four primes
;; with this property.
;;
;; Find the lowest sum for a set of five primes for which any two primes
;; concatenate to produce another prime.
;;
;; Answer:


;;;(load "png.scm")
;;;(define *png* (make-primes-generator))
;;;(define (prime? n) (*png* 'prime? n))
;;;(define (first-prime) (*png* 'first))
;;;(define (next-prime) (*png* 'next))

(load "miller-rabin-primality-test.scm")

;; Return number of decimal digits number n is consists of
;;
(define (decimal-digits n)
  (inexact->exact (ceiling (log10 (+ n 1)))))


;; Return list of pairs where each pair is made of consecutive 
;; digits of the n and every pair member is prime.
;;
;; Example:
;;   (number->prime-pairs 88397) -> ((8839 . 7) (883 . 97))
;;
(define (number->prime-pairs n)
  (let iter ((rdigits 1)
             (ldigits (- (decimal-digits n) 1))
             (res     '()))
    (if (zero? ldigits)
      res
      (let* ((rtens (integer-expt 10 rdigits))
             (ltens (integer-expt 10 ldigits))
             (r     (remainder n rtens))
             (l     (quotient n rtens))
             (c     (+ l (* r ltens))))
        (iter (+ rdigits 1)
              (- ldigits 1)
              (if (and (every prime? (list r l c))
                       (> r (if (= 1 rdigits) 0 (integer-expt 10 (- rdigits 1)))))
                (cons (cons l r) res)
                res))))))


(define (pair-less x y)
  (let ((carx (car x))
        (cdrx (cdr x))
        (cary (car y))
        (cdry (cdr y)))
    (cond ((< carx cary)
           #t)
          ((> carx cary)
           #f)
          (else
            (< cdrx cdry)))))


(define (unique ls)
  (if (null? ls)
    ls
    (let loop ((in  (cdr ls))
               (out (list (car ls))))
      (cond ((null? in)
             (reverse out))
            ((equal? (car in) (car out))
             (loop (cdr in) out))
            (else
              (loop (cdr in) (cons (car in) out)))))))


;; Generate
;;
(define (f1 max-number)
  (let loop ((res '())
             (n   2))
    (cond ((> n max-number)
           res)
          ((prime? n)
           (loop
             (append (number->prime-pairs n) res)
             (+ n 1)))
          (else
            (loop res (+ n 1))))))


;; Sort pair elements
;;
(define (f2 x less)
  (map (lambda (y)
         (let ((cary (car y))
               (cdry (cdr y)))
           (if (less cary cdry)
             y
             (cons cdry cary))))
       x))


;; Pack
;;
(define (f3 x)
  (let loop ((in   (cdr x))
             (n    (car (car x)))
             (t    (list (cdr (car x))))
             (out '()))
    (cond ((null? in)
           (reverse (cons (cons n (reverse t)) out)))
          ((= n (car (car in)))
           (loop
             (cdr in)
             n
             (cons (cdr (car in)) t)
             out))
          (else
            (loop
              (cdr in)
              (car (car in))
              (list (cdr (car in)))
              (cons (cons n (reverse t)) out))))))


(define (sublist? subls ls)
  (cond ((null? subls)
         #t)
        ((null? ls)
         #f)
        (else
          (let ((m (member (car subls) ls)))
            (if m
              (sublist? (cdr subls) m)
              #f)))))


(define (print ls)
  (for-each (lambda (i) (format #t "~a\n" i)) ls))


(define (j1 ls len)

  (define (j4 sub ls)
    (if (null? ls)
      #f
      (let loop ((x (filter (lambda (i) (sublist? sub i)) ls)))
        (if (null? x)
          #f
          (if (= len (length sub))
            (cons (car (car x)) sub)
            (or (j4 (cons (car (car x)) sub) (cdr x))
                (loop (cdr x))))))))

  (define (j2 x y)

    (define (j3 n nn)
      (let loop ((nn nn))
        (if (null? nn)
          #f
          (or (j4 (list n (car nn)) y) (loop (cdr nn))))))

    (if (null? y)
      #f
      (j3 (car x) (cdr x))))

  (let loop ((x ls))
    (if (null? x)
      #f
      (or (j2 (car x) (cdr x))
          (loop (cdr x))))))


(define (xx max-number)
  (let* ((l1 (f1 max-number))
         (l2 (f2 l1 >))
         (l3 (unique (sort l2 (lambda (x y) (pair-less y x)))))
         (l4 (f3 l3))
         (res l4))
    res))


(define (concat-numbers a b)
  (let loop ((m 10))
    (if (< b m)
      (+ (* a m) b)
      (loop (* m 10)))))


(define (f10 max-num)
  (let loop ((n 8)
             (res '()))
    (if (> n max-num)
      res
      (loop (+ n 1)
            (if (and (prime? n ) 
                     (every prime?
                            (list (concat-numbers n 3)
                                  (concat-numbers 3 n)
                                  (concat-numbers n 7)
                                  (concat-numbers 7 n))))
              (cons n res)
              res)))))

(define (f11 ls len)

  (define (f12 n ls)
    (format #t "f12: ~a ~a\n" n ls)
    (if (null? ls)
      #f
      (let ((x (filter (lambda (i) (every prime? 
                                          (list (concat-numbers n i)
                                                (concat-numbers i n))))
                       ls)))
        (format #t "f12: x ~a\n" x)
        (if (null? x)
          #f
          (let ((y (f11 x (- len 1))))
            (if y
              (cons n y)
              #f))))))

  ;; f11
  (format #t "f11: ~a ~a\n" ls len)
  (cond ((null? ls)
         #f)
        ((zero? len)
         ls)
        (else
          (or (f12 (car ls) (cdr ls)) 
              (f11 (cdr ls) len)))))

;; end of file
