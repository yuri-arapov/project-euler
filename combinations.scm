;; combinations.scm
;;
;; Unique combinations of list elements.
;;


;; Return number of unique combination of m elements by n.
;;
(define (combinations-count m n)
  (if (= 1 n)
    m
    (* (/ m n) (combinations-count (- m 1) (- n 1)))))


;; Return list of unique combinations of ls list elements by n.
;;
(define (combinations ls n)
  (define (iter ls n)
    (cond ((zero? n)
           '())
          ((< (length ls) n)
           '())
          ((= n 1)
           (map (lambda (x) (list x)) ls))
          ((= (length ls) n)
           (list ls))
          (else
            (let* ((r1 (iter (cdr ls) (- n 1)))
                   (r2 (iter (cdr ls) n)))
              (append
                (map (lambda (x) (cons (car ls) x)) r1)
                r2)))))
  (iter ls n))


;; end of file
