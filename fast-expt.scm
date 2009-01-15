;;


(define (fast-expt base power)
  (let loop ((res 1) (b base) (p power))
    (cond ((> res 10000000000)
           (loop (remainder res 10000000000) b p))

          ((= p 0)
           res)

          ((even? p)
           (loop res (* b b) (/ p 2)))

          (else
            (loop (* res b) b (- p 1))))))



;; end of file
