;; Quadratic equation solver
;;
;;     2
;;   ax  + bx + c = 0
;;
;; See http://en.wikipedia.org/wiki/Quadratic_equation
;;


(define (quadratic-equation a b c)
  (cond
    ((zero? a) ;; linear equation
     (cond 
       ((not (zero? b)) ((lambda (x) (values x x)) (/ (- c) b)))
       (else            (values #f #f))))
    (else
      (or 
        (and-let* 
          ((dd (- (* b b) (* 4 a c)))
           ((not (negative? dd)))
           (d (sqrt dd))
           (a2 (* a 2))
           (x1 (/ (- (- b) d) a2))
           (x2 (/ (+ (- b) d) a2)))
          (if (< x1 x2)
            (values x1 x2)
            (values x2 x1)))
        (values #f #f)))))


;; end of file
;; vim: ts=4 sw=4
