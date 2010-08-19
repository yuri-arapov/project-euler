;; System of linear equations.
;;
;; Method: Gaussian elimination.
;; See http://en.wikipedia.org/wiki/Gaussian_elimination
;;
;; NOTE: developed for and tested on integer data.
;;
;; Written by Yuri Arapov <yuridichesky@gmail.com>


(load "integer-square-root.scm")
(load "group-by.scm")


;; Print list contents: one element per line.
;;
(define (ls:print s)
  (for-each (lambda (i) (format #t "~a\n" i)) s))


;; Divide each row element by GCD of the row.
;;
(define (ls:row-normalize row)
  (let ((d (apply gcd row)))
    (if (not (zero? d))
      (map (lambda (n) (/ n d)) row)
      row)))


;; Multiply row by given x.
;;
(define (ls:row* row x)        (map (lambda (n) (* n x)) row))

;; Subtract row2 from row1: res=row1-row2.
;;
(define (ls:rows- row1 row2)   (map (lambda (x y) (- x y)) row1 row2))


;; Gaussian elimination for given row.
;; Index refers to diagonal element.
;;
(define (ls:eliminate pivot row index)
  (ls:row-normalize
    (ls:rows-
      (ls:row* row   (list-ref pivot index))
      (ls:row* pivot (list-ref row   index)))))


;; Gaussian elimination for entire matrix.
;; start-index refers to diagonal element of the first row.
;; The order of resultant rows is reversed.
;;
(define (ls:pass start-index s)
  (let loop ((in-rows  (cdr s))
             (index    start-index)
             (pivot    (ls:row-normalize (car s)))
             (out-rows '()))
    (cond 
      ((null? in-rows)
       (cons pivot out-rows))

      (else
        (let ((tmp (map (lambda (row)
                          (ls:eliminate pivot row index))
                        in-rows)))
          (loop (cdr tmp)
                (1+ index)
                (car tmp)
                (cons pivot out-rows)))))))


;; First pass of linear system solving:
;; eliminate matrix elements left from diagonal.
;; Input: initial matrix.
;; Output: reversed rows (first input row will be 
;;         the last output row).
;;
(define (ls:pass1 s)
  (ls:pass 0 s))


;; Second pass of linear system solving:
;; eliminate matrix elements right from diagonal.
;; Input: immediate result of pass1 (i.e. first row is the
;;        most zeroed one).
;; Output: diagonal matrix, same rows order as initial matrix.
;;
(define (ls:pass2 s)
  (define (reverse-columns s) (map (lambda (i) (reverse i)) s))
  (reverse-columns
    (ls:pass
      1 ;; first element of each row is free member (bi) because
        ;; columns were reversed;
        ;; so number of diagonal element of first row is 1
      (reverse-columns s))))


;; Third pass of linear system solving:
;; compute result list.
;; Input: immediate result of pass2 (i.e. diagonal matrix and
;;        column of free members).
;; Return #f if some of the diagonal elements is zero.
;;
(define (ls:pass3 s)
  (define (dimension s) (1- (length (car s))))
  (define (safe-div n divisor) (if (zero? divisor) #f (/ n divisor)))
  (let ((res (map (lambda (row index)
                    (safe-div (last row) (list-ref row index)))
                  s
                  (iota (dimension s)))))
    (if (every number? res) res #f)))



;; Compute dimension of the linear system from length of input data.
;; Input: plain list representing linear system matrix:
;;        (a11 a12 ... a1n b1
;;         ...
;;         an1 an2 ... ann bn)
;; Return either n or #f in case if there is no such n so that
;; n*(n+1) = length(s)
;;
;; Formulae:
;;                     2            
;;   n*(n+1)=L  -->   n  + n - L = 0
;;
;; The general form of quadratic equation is
;;     2
;;   ax  + bx + c = 0
;;
;; In this case a=1, b=1, c=-L, so we need the only solution of 
;; quadratic equation:
;;      ------
;;     V 1+4L  - 1
;;   n=-----------, and solution must be integer.
;;           2
;;
(define (ls:length->dimension l)
  (and-let* ((dd (+ 1 (* 4 l)))
             (d  (integer-square-root dd))
             ((= (* d d) dd))
             ((odd? d)))
            (/ (- d 1) 2)))


;; Solve linear system.
;; Input: plain list representing linear system matrix:
;;        (a11 a12 ... a1n b1
;;         ...
;;         an1 an2 ... ann bn)
;; Return either (x1 x2 ... xn) list or #f if there's no solution or
;; input data do not represent Nx(N+1) matrix.
;;
;; NOTE: developed for and tested on integer data.
;;
(define (linear-system s)
  (and-let* ((n (ls:length->dimension (length s)))
             (m (group-by (1+ n) s))
             (t (ls:pass2 (ls:pass1 m)))
             (r (ls:pass3 t)))
            r))


(define (ls:test-data)
  '( 2  1 -1    8
    -3 -1  2  -11
    -2  1  2   -3))


(define (ls:test-matrix)
  (group-by 4 (ls:test-data)))


(define (ls:test-pass1)
  (ls:print
    (ls:pass1 (ls:test-matrix))))


(define (ls:test-pass1-pass2)
  (ls:print
    (ls:pass2 (ls:pass1 (ls:test-matrix)))))


;; end of file
;; vim: ts=4 sw=4
