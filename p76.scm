;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=76
;;
;; Problem 76
;; 13 August 2004
;;
;; It is possible to write five as a sum in exactly six different ways:
;;
;; 4 + 1
;; 3 + 2
;; 3 + 1 + 1
;; 2 + 2 + 1
;; 2 + 1 + 1 + 1
;; 1 + 1 + 1 + 1 + 1
;;
;; How many different ways can one hundred be written as a sum of at least two
;; positive integers?
;;
;; Answer: ???


(define shift 0)

(define fmt format)

(define (print-shift)
  (let loop ((s shift))
    (if (zero? s)
      #t
      (begin
        (fmt #t " ")
        (loop (- s 1))))))

(define (format . z)
  (print-shift)
  (apply fmt z))


(define (make-shifted-proc p)
  (lambda (. z)
    (set! shift (+ shift 4))
    (let ((res (apply p z)))
      (set! shift (- shift 4))
      res)))

        

(define (sum-as n right)
  (cond ((< n right)
         (format #t "s=0~%")
         0)
        ((= n right)
         (format #t "s=1~%")
         1)
        (else
          (format #t "sum-as: ~a|~a~%" n right)
          (let loop ((s 0)
                     (l (- n 1))
                     (r 1))
            (format #t "~a=~a+~a~%" (+ l r) l r)
            (cond ((< l r)
                   (format #t "s=~a~%" s)
                   s)
                  (else
                    (loop (+ s (sum-as l r) (sum-as r right)) (- l 1) (+ r 1))))))))


(define (p76)
  (sum-as 6))


;; end of file
