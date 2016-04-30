;; Feb. 25th, 2015
;;


(define (divider? x d) (zero? (remainder x d)))


(define (p153-limit limit)
  (let loop-x ((x 1) (res 0))
    (if (> x limit) res
      (loop-x (1+ x) (+ res
                        (let loop-a ((a 1) (res 0))
                          (if (> a x ) res
                            (loop-a (1+ a) (+ res
                                              (let loop-b ((b 0) (res 0))
                                                (let ((aabb (+ (* a a) (* b b)))
                                                      (xa   (* x a))
                                                      (xb   (* x b)))
                                                  (if (> aabb xa) res
                                                    (if (and (divider? xa aabb) (divider? xb aabb))
                                                      (begin
                                                        (format #t "~a ~a ~a\n" x a b)
                                                        (loop-b (1+ b) (if (zero? b) (+ res a) (+ res (* a 2)))))
                                                      (loop-b (1+ b) res))))))))))))))


(define (gaussian-dividers n)
  (let loop ((a 1) (b 1) (res (list (list 1 0))))
    (let ((aabb (+ (* a a) (* b b)))
          (na   (* n a))
          (nb   (* n b)))
      (cond 
        ((> (* 2 a) n) (reverse (cons (list n 0) res)))

        ((> b n) (loop (1+ a) 0 res))

        ((and (divider? na aabb) (divider? nb aabb))
         (loop a (1+ b) (cons (list a b) res)))

        (else
          (loop a (1+ b) res))))))


;; end of file
