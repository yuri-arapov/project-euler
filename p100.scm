;; 2010-08-05
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=100
;;
;; Problem 100
;; 15 July 2005
;;
;; If a box contains twenty-one coloured discs, composed of fifteen blue discs
;; and six red discs, and two discs were taken at random, it can be seen that
;; the probability of taking two blue discs, P(BB) = (15/21)×(14/20) = 1/2.
;;
;; The next such arrangement, for which there is exactly 50% chance of taking
;; two blue discs at random, is a box containing eighty-five blue discs and
;; thirty-five red discs.
;;
;; By finding the first arrangement to contain over 10^(12) = 1,000,000,000,000
;; discs in total, determine the number of blue discs that the box would
;; contain.
;;
;; Answer:
;;
;; Formulae:
;;
;; Let total number of disck be N.
;; Let number of blue ones be B.
;;
;; Then
;;   B/N x (B-1)/(N-1) = 1/2
;;
;; So
;;   2 x B x (B - 1) = N x (N - 1)
;;
;; Or
;;     2          2
;;   2B  - 2B - (N - N) = 0
;;
;; or
;;     2         2
;;   2B  - 2B - N  + N = 0       (***)
;;
;; Solution of quadratic equation 
;; (see http://en.wikipedia.org/wiki/Quadratic_equation):
;;
;;     2
;;   ax  + 2kx + c = 0
;;
;; is             ______
;;               / 2
;;       -k +/- V k - ac
;;   x = ----------------
;;              a
;;
;; In our case:
;;   a = 2
;;
;;   k = -1
;;            2
;;   c = N - N
;;              
;; So            ------------
;;              /       2
;;       1 +/- V 1 + 2(N - N)
;;   x = --------------------
;;               2
;;
;; The x must be integer and positive, so
;;
;;             ------------
;;            /       2
;;       1 + V 1 + 2(N - N)
;;   x = ------------------, and
;;               2
;;
;;     ------------
;;    /       2
;;   V 1 + 2(N - N) == integer and odd
;;
;;          2
;;   1 + 2(N - N) == perfect square and odd
;;
;;
;; NOTE: (***) is an Diophantine Quadratic Equation, see
;;       http://en.wikipedia.org/wiki/Diophantine_equation
;;       http://blog.dreamshire.com/2009/05/27/project-euler-problem-100-solution/
;;       http://www.alpertron.com.ar/QUAD.HTM
;;



(load "integer-square-root.scm")



(define (f n) (+ 1 (* 2 (- (* n n) n))))



(define (match? f) (and (odd? f) (square? f)))



(define (p100-start start)
  (let loop ((n start))
    (if (zero? (remainder n 1000))
      (format #t "~a\n" n))
    (or 
      (and-let* ((i (f n))
                 ((match? i)))
                (/ (+ 1 (integer-square-root i)) 2))
      (loop (1+ n)))))


(define (p100) (p100-start (1+ (expt 10 12))))


;; end of file
;; vim: ts=4 sw=4
