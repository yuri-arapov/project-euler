;; 2011-05-03
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=140
;;
;; Problem 140
;; 03 February 2007
;;
;; Consider the infinite polynomial series AG(x) = xG1 + x^2G2 + x^3G3 + ...,
;; where Gk is the kth term of the second order recurrence relation G(k) =
;; G(k-1) + G(k-2), G(1) = 1 and G(2) = 4; that is, 1, 4, 5, 9, 14, 23, ... .
;;
;; For this problem we shall be concerned with values of x for which AG(x) is a
;; positive integer.
;;
;; The corresponding values of x for the first five natural numbers are shown below.
;;
;;       x       AG(x)
;;    (51)/4       1
;;      2/5        2 (golden nugget)
;;    (222)/6      3
;;   (1375)/14     4
;;      1/2        5 (golden nugget)
;;
;; We shall call AG(x) a golden nugget if x is rational, because they become
;; increasingly rarer; for example, the 20th golden nugget is 211345365.
;;
;; Find the sum of the first thirty golden nuggets.
;;
;; Answer: 5673835352990


(load "square-root-cont-fract.scm")


;; Compute fundamental solution of Pell's equation
;;
;;    2     2
;;   x  - Dy  = 1
;;
;; See http://en.wikipedia.org/wiki/Pell_equation for details.
;;
;; Example:
;;   (pell-equation 2) -> (3 . 2)
(define (pell-equation D)
  (let loop ((n 1))
    (let ((e (expand-cont-fract (square-root-cont-fract D n))))
      (let ((x (numerator e))
            (y (denominator e)))
        (if (= 1 (- (sqr x) (* D (sqr y))))
          (cons x y)
          (loop (1+ n)))))))


;; Return list of count solutions of Pell's equations based on
;; fundamental solution (x1 y1) of Pell's equation
;;
;;    2     2
;;   x  - ny  = 1
;;
;; Example:
;;   (pell-equation-solutions 2 3 2 5) -> 
;;   ((3 . 2) (17 . 12) (99 . 70) (577 . 408) (3363 . 2378))
(define (pell-equation-solutions n x1 y1 count)
  (let loop ((c (1- count))
             (res (list (cons x1 y1))))
    (if (zero? c)
      (reverse res)
      (let ((xk (car (car res)))
            (yk (cdr (car res))))
        (loop (1- c) (cons (cons (+ (* x1 xk) (* n y1 yk))
                                 (+ (* x1 yk) (* y1 xk)))
                           res))))))


;; Compute list of fundamental solutions of general Pell's equation
;;
;;    2     2
;;   x  - Dy  = N
;;
;; See www.jpr2718.org/pell.pdf for details.
(define (general-pell-equation D N)

  ;; True if x is perfect square.
  (define (square? x) (integer? (sqrt x)))

  ;; Integer square root.
  (define (int-sqrt x) (inexact->exact (sqrt x)))

  (let* ((tu (pell-equation D))
         (t (car tu))
         (u (cdr tu)))
    (let ((L1 0)
          (L2 (sqrt (/ (* N (- t 1)) (* 2 D)))))
      (let loop ((y L1)
                 (res '()))
        (if (> y L2)
          (sort res (lambda (a b) (< (car a) (car b))))
          (let ((xx (+ N (* D y y))))
            (if (square? xx)
              (let ((x (int-sqrt xx)))
                (let ((x2 (+ (* x t) (* (- y) u D)))
                      (y2 (+ (* x u) (* (- y) t))))
                  (loop (1+ y) (cons* (cons x y) (cons x2 y2) res))))
              (loop (1+ y) res))))))))


;; Compute additional solution of general Pell equation 
;;
;;    2     2
;;   x  - Dy  = N
;;
;; from one of its fundamental solutions (r s) and some solution (t u) of
;; corresponding Pell equation
;;
;;    2     2
;;   x  - Dy  = 1
;;
;; See www.jpr2718.org/pell.pdf for details.
(define (general-pell-equation-solutions r s D t u)
  (cons (+ (* r t) (* s u D))
        (+ (* r u) (* s t))))


;; Compute list of solutions of general Pell equation
;;
;;    2     2
;;   x  - Dy  = N
;;
;; from one of its fundamental solutions (rs) and list of 
;; solutions (tu-list) of corresponding Pell equation
;;
;;    2     2
;;   x  - Dy  = 1
(define (generate rs D tu-list)
  (let ((r (car rs))
        (s (cdr rs)))
    (cons rs
          (map (lambda (tu)
                 (let ((t (car tu))
                       (u (cdr tu)))
                   (general-pell-equation-solutions r s D t u)))
               tu-list))))


;; Solve problem 140.
;;
;; The generating formula for 1, 4, 5, 9, 14, 23... sequence is 
;;
;;   (1+3*x)/(1-x-x^2)                                                       (1)
;;
;; (see http://oeis.org/A000285).
;;
;; So the AG(x) will be
;; 
;;   AG(x) = x*(1+3*x)/(1-x-x^2)                                             (2)
;;
;; Solve (2) to find x (in maxima):
;;
;; (%i1) solve(A=x*(1+3*x)/(1-x-x^2), x);
;;                      2                                  2
;;              sqrt(5 A  + 14 A + 1) + A + 1      sqrt(5 A  + 14 A + 1) - A - 1
;; (%o1) [x = - -----------------------------, x = -----------------------------]
;;                        2 A + 6                            2 A + 6
;;
;; Since x is rational then (5A^2+14A+1) must be perfect square:
;;
;;   5A^2+14A+1=D^2                                                          (3)
;;
;; Solve (3) to find A (in maxima):
;;
;; (%i2) solve(D^2=5*A^2+14*A+1, A);
;;                             2                        2
;;                     sqrt(5 D  + 44) + 7      sqrt(5 D  + 44) - 7
;; (%o2)        [A = - -------------------, A = -------------------]
;;                              5                        5
;;
;; Since A must be natuaral number then (5D^2+44) must be perfect square:
;; 
;;   5D^2+44=Q^2                                                             (4)
;;
;; Or
;;
;;   Q^2 - 5D^2 = 44                                                         (5)
;;
;; which is general Pell equation.
;;
;; So the method is:
;; 1. Determine some number of solutions of (5) -- get a list of (Q D) pairs.
;; 2. Then use Q to compute corresponding list of A.
;; 3. Then filter natural A (positive integers).
;; 4. If there are enough number of A, then compute sum of first 30 A 
;;    and stop.
;; 5. Otherwise increase number of solutions of (5) and start over again.
(define (p140)

  (define (q->a q) (/ (- q 7) 5))
  (define (natural? n) (and (integer? n) (positive? n)))

  (let* ((tu (pell-equation 5))
         (t  (car tu))
         (u  (cdr tu))
         (qd (general-pell-equation 5 44)))
    (let loop ((n 0))
      (if (= n 30) ;; I hope 30 additional solutions of Pell equation will
                   ;; be enough; and if problem is still not solved then
                   ;; there's something wrong with the algorithm.
        #f
        (let* ((tu-add (if (zero? n) '() (pell-equation-solutions 5 t u n)))
               ;; fundamental and additional solutions of Pell equation, 
               ;; total number of solutions is n

               (qd-add (apply append (map (lambda (i) (generate i 5 tu-add)) qd)))
               ;; fundamental and additional solutions of general Pell
               ;; equation

               (A (sort (filter natural? (map q->a (map car qd-add))) <)))
               ;; list of natural A

          (if (>= (length A) 30)
            (values (apply + (take A 30)) n)
            (loop (if (zero? n) 1 (* n 2)))))))))


;; end of file
;; vim: sw=4 ts=4
