;; fac.scm
;;
;; factorial


(define (fac n)
;; return n!
;;
;; (iterative version)
;;
  (define (iter res n)
    (if (< n 2)
      res
      (iter (* res n) (- n 1))))
  (iter 1 n))


;; end of file
