;; union.scm
;;
;; merge two sorted lists to make a union of l1 and l2
;; (i.e. each list item will be stored in resultant union just once)

(define (union l1 l2)
  (define (iter res l1 l2)
    (cond ((null? l1) (append res l2))
          ((null? l2) (append res l1))
          (else
            (let ((i1 (car l1))
                  (i2 (car l2)))
                 (cond ((= i1 i2) (iter (append res (list i1)) (cdr l1) (cdr l2)))
                       ((< i1 i2) (iter (append res (list i1)) (cdr l1)      l2))
                       ((> i1 i2) (iter (append res (list i2))      l1  (cdr l2))))))))
  (iter '() l1 l2))

;; end of file
