
(define (increment x)
  (if (null? x) '(1)
    (let ((n (car x)))
      (if (< n 6) (cons (1+ n) (cdr x))
        (cons 0 (increment (cdr x)))))))
