;; number-digits.scm
;;
;; transform integer number to the list of digits
;;
;; example:
;;
;;   (number->digits 123) : (3 2 1)


(define (number->digits n)
;; return list of digits this number is made of
;; (right to left: lower digits go first)
;;
;;  (define (iter ls n)
;;    (if (< n 10)
;;      (cons n ls)
;;      (iter (cons (remainder n 10) ls) (quotient n 10))))
;;  (iter '() n))
  (if (< n 10)
    (list n)
    (cons (remainder n 10) 
          (number->digits (quotient n 10)))))

(define (number->digits n)
  (if (< n 10)
    (list n)
    (cons (number->digits (quotient n 10)) (list (remainder n 10)))))

;; end of file
