;; number-digits.scm
;;
;; transform integer number to the list of digits and vise versa
;;
;; example:
;;
;;   (number->digits 123) : (1 2 3)


(define (number->digits n)
;; return list of decimal digits this number is made of
;; (higher digits first)
;;
  (define (iter ls n)
    (if (< n 10)
      (cons n ls)
      (iter (cons (remainder n 10) ls) (quotient n 10))))
  (iter '() n))
;;  (if (< n 10)
;;    (list n)
;;    (cons (remainder n 10) 
;;          (number->digits (quotient n 10)))))

;;(define (number->digits n)
;;  (if (< n 10)
;;    (list n)
;;    (cons (number->digits (quotient n 10)) (list (remainder n 10)))))


(define (digits->number dd)
  (reduce (lambda (d res) (+ (* res 10) d)) 0 dd))


;; end of file
