;; 20-Feb-2008
;;
;; Structure and Interpretation of Computer Programs
;; 
;; test the number for being the prime


(define (first-divisor x)
  (define (next n)
    (if (= n 2)
      3
      (+ n 2)))

  (define (test d x)
    (if (> (* d d) x)
      x
      (if (zero? (remainder x d))
        d
        (test (next d) x))))
  (test 2 x))


(define (prime? x)
  (= (first-divisor x) x))


;; end of file
