;; diophantine.scm
;;
;; Solve Diophantine equation of 1-st degree:
;;
;; Ax + By = C
;;
;; Source:
;;   http://ranmath.webng.com/diophan/diophan.htm


;; Solve Diophantine equation Ax + By = C.
;; Return: 
;;   * false          -- if no solutions exists;
;;   * (false.Y)      -- when A is zero and B is divisor of C;
;;   * (X.false)      -- when B is zero and A is divisor of C;
;;   * (f t) -> (x.y) -- solution generating function.
(define (diophantine A B C)

  (define (swap-xy xy) (cons (cdr xy) (car xy)))

  (define (swap-if-pair xy) (if (pair? xy) (swap-xy xy) xy))

  (define (sign x) (if (positive? x) 1 -1))

  (define (solve A B C Fxy)
    (cond 
      ((let ((D (gcd A B)))
         (and (> D 1) (> (remainder C D) 0)))
       ;; no solution: non-trivial D=GCD(A,B) must be divisor of C too.
       #f)

      ((let ((D (gcd A B C)))
         (> D 1))
       ;; reduce coefficients of the equation.
       (solve (/ A D) (/ B D) (/ C D) Fxy))

      ((> (abs A) (abs B))
       ;; reorder variables.
       (solve B A C (lambda (xy) (Fxy (swap-xy xy)))))

      ((negative? A)
       ;; invert coefficients.
       (solve (- A) (- B) (- C) Fxy))

      ((> A 1)
       ;; reduce coefficients by introducing new variable (z).
       (let ((Q (quotient (abs B) A))
             (R (remainder (abs B) A)))
         (solve 
           (* R (sign B)) A C 
           (lambda (yz)
             (let* ((y (car yz))
                    (z (cdr yz))
                    (x (- z (* Q y (sign B)))))
               (Fxy (cons x y)))))))

      (else 
        ;; A is 1, make solution generator.
        (lambda (y)
          (Fxy (cons (- C (* B y)) y))))))

  (cond
    ((and (zero? A) (zero? B))
     #f)

    ((and (zero? B) (not (zero? A)))
     (let ((C/A (/ C A)))
       (if (integer? C/A)
         (cons C/A #f)
         #f)))

    ((and (zero? A) (not (zero? B)))
     (swap-if-pair
       (diophantine B A C)))

    (else 
      (solve A B C identity))))


;; end of file
;; vim: sw=4 ts=4
