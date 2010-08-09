;; ingeger square root
;;


;; Return lower and upper bound of square root of n:
;;   b1^2 <= n < b2^2
;;
(define (square-root-bounds n)
  (let loop ((x 1))
    (if (> (* x x) n)
      (values (quotient x 2) x)
      (loop (* x 2)))))



;; Compute integer suare root of n:
;;   s^2 <= n < (s+1)^2
;;
(define (integer-square-root n)
  (let-values (((a b) (square-root-bounds n)))
    (let loop ((a a) (b b))
      (if (<= (- b a) 1)
        a
        (let ((c (quotient (+ a b) 2)))
          (if (<= (* c c) n)
            (loop c b)
            (loop a c)))))))



;; Return true if n is a square number.
;;
(define (square? n)
  (let ((x (integer-square-root n)))
    (= n (* x x))))


;; end of file
;; vim: ts=4 sw=4
