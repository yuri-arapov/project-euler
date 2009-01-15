;; accumulate.scm
;;
;; do some accumulation on some sequene of numbers


(define (filtered-accumulate op term filter? next init from to)
  (define (iter res n)
    (if (> n to) 
      res
      (let ((t (term n)))
            (iter (if (filter? t) (op res t) res) (next n)))))
  (iter init from))


(define (accumulate op term next init from to)
  (filtered-accumulate 
    op
    term
    (lambda (n) #t)
    next
    init
    from
    to))


;; end of file
