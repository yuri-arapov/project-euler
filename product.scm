;; each-to-each
;; Apply operation op for each pair of elements from s1 and s2,
;; return list of resultant values
;;
(define (each-to-each s1 s2 op)
  (let loop ((i1 s1)
             (i2 s2)
             (res '()))
    (cond ((null? i1)
           res)
          ((null? i2)
           (loop (cdr i1) s2 res))
          (else
            (loop i1 (cdr i2) (cons (op (car i1) (car i2)) res))))))


;; Join elements of the lists to each other:
;; (product '(a b) '(c d e)) ->
;; ((a c) (a d) (a e) (b c) (b d) (b e))
;;
(define (product s1 s2)
  (define (tolist i) (if (list? i) i (list i)))
  (each-to-each s1 s2 (lambda (i1 i2) (append (tolist i1) (tolist i2)))))

;; end of file
