;; 28 March 2007
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=66
;;
;; Problem 66
;; 26 March 2004
;;
;; Consider quadratic Diophantine equations of the form:
;;
;; x^2 – Dy^2 = 1
;;
;; For example, when D=13, the minimal solution in x is 6492 – 13×1802 = 1.
;;
;; It can be assumed that there are no solutions in positive integers when D is
;; square.
;;
;; By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the
;; following:
;;
;; 3^2 – 2×2^2 = 1
;; 2^2 – 3×1^2 = 1
;; 9^2 – 5×4^2 = 1
;; 5^2 – 6×2^2 = 1
;; 8^2 – 7×3^2 = 1
;;
;; Hence, by considering minimal solutions in x for D ≤ 7, the largest x is
;; obtained when D=5.
;;
;; Find the value of D ≤ 1000 in minimal solutions of x for which the largest
;; value of x is obtained.
;;
;; Answer: ???
;;


(load "print.scm")


(define (int-part x)
  (inexact->exact (floor x)))


(define (number->cont-fraction n maxlen)
  (define (iter n len res)
    (cond ((zero? len) res)
          ((zero? n) res)
          (else
            (let* ((nn (/ 1.0 n))
                   (in (int-part nn)))
              (iter (- nn in) (- len 1) (append res (list in)))))))
  (if (zero? n)
    '(0)
    (let ((in (int-part n)))
      (iter (- n in) (- maxlen 1) (list in)))))


(define (make-cont-fraction-generator x)
  (let* ((index 0)
         (x     x)
         (r     0)
         (a     0))
    (lambda () 
;;      (println index " " x)
      (cond ((zero? x)
             #f)
            ((= index 0)
              (set! a (int-part x))
              (set! x (- x a))
              (set! index (+ index 1))
              a)
            (else
              (set! r (/ 1 x))
              (set! a (int-part r))
              (set! x (- r a))
              (set! index (+ index 1))
              a)))))


(define (cont-fraction->number a)
  (define (save-div x y)
    (if (zero? y)
      0
      (/ x y)))
  (if (null? a)
    0
    (+ (car a) (save-div 1.0 (cont-fraction->number (cdr a))))))


(define (cont-fraction-prec n maxlen)
  (abs (- (cont-fraction->number (number->cont-fraction n maxlen)) n)))


(define (cont-fraction->convergents a)

  (define (iter hn-1 kn-1 hn-2 kn-2 a res)
    (if (null? a)
      res
      (let* ((an (car a))
             (hn (+ (* an hn-1) hn-2))
             (kn (+ (* an kn-1) kn-2)))
        (iter hn-1 kn-1 hn kn (cdr a) (append res (list (list hn kn)))))))

  (cond ((null? a)
         '()) ;; msut not happen generally speaking
        ((= 1 (length a))
         (list (list (car a) 1)))
        (else
          (let* ((a0 (car a))
                 (a1 (cadr a))
                 (h1 a0)
                 (k1 1)
                 (h2 (+ (* a1 a0) 1))
                 (k2 a1))
            (iter h2 k2 h1 k1 (cddr a) (list (list h1 k1) (list h2 k2)))))))


(define (make-convergents-generator cont-fraction-generator)
  (let* ((cfg   cont-fraction-generator)
         (a0    (cfg))
         (a1    (cfg))
         (h1    a0)
         (k1    1)
         (h2    (+ (* a1 a0) 1))
         (k2    a1)
         (hn-1  0) (kn-1  0)
         (hn-2  0) (kn-2  0)
         (index 1))
    (lambda ()
      (cond ((= 1 index)
;;              (println a0 " " a1)
              (set! index (+ index 1))
              (list h1 k1))
            ((= 2 index)
              (set! index (+ index 1))
              (set! hn-2 h1)
              (set! kn-2 k1)
              (set! hn-1 h2)
              (set! kn-1 k2)
              (list h2 k2))
            (else
              (set! index (+ index 1))
              (let* ((an (cfg))
                     (hn (+ (* an hn-1) hn-2))
                     (kn (+ (* an kn-1) kn-2)))
                (set! hn-2 hn-1)
                (set! kn-2 kn-1)
                (set! hn-1 hn)
                (set! kn-1 kn)
                (list hn kn)))))))


(define (quadratic? n)
  (integer? (sqrt n)))


(define (pell-value d x y) (- (* x x) (* d y y)))


(define (pell-solution? d x y)  (= 1 (pell-value d x y)))


(define (pell-equation d)

  (if (quadratic? d)
    #f
    (let* ((srd (sqrt d))
           (a   (number->cont-fraction srd 30))
           (h/k (cont-fraction->convergents a)))
      (let loop ((h/k h/k))
        (if (null? h/k)
          #f
          (let ((h (car (car h/k)))
                (k (cadr (car h/k))))
            (if (pell-solution? d h k)
              (list h k)
              (loop (cdr h/k)))))))))


(define (pell-equation2 d)

  (if (quadratic? d)
    #f
    (let* ((cfg (make-cont-fraction-generator (sqrt d)))
           (hkg (make-convergents-generator cfg)))
      (let loop ()
        (let* ((h/k (hkg))
               (x (car h/k))
               (y (cadr h/k)))
          (if (pell-solution? d x y)
            (list x y)
            (loop)))))))


;; end of file
