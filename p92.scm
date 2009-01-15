;; 29 Apr. 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=92
;;
;; Problem 92
;; 01 April 2005
;;
;; A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.
;;
;; For example,
;;
;; 44 → 32 → 13 → 10 → 1 → 1
;; 85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89
;;
;; Therefore any chain that arrives at 1 or 89 will become stuck in an endless
;; loop. What is most amazing is that EVERY starting number will eventually
;; arrive at 1 or 89.
;;
;; How many starting numbers below ten million will arrive at 89?
;;
;; Answer: ???
;;


(define (number->digits n)
  (if (< n 10)
    (list n)
    (cons (remainder n 10) (number->digits (quotient n 10)))))


(define (digits->number ls)
  (fold-right (lambda (x res) (+ x (* res 10))) 0 ls))



(define (limit->9s limit)
  (let ((dd (number->digits limit)))
    (digits->number (map (lambda (x) 9) dd))))



(define (number->square-of-digits n)
  (apply + (map (lambda (x) (* x x)) (number->digits n))))



(define (p92- limit)
  (let* ((ll (limit->9s limit))
         (nn (number->square-of-digits ll))
         (tt (make-vector (+ nn 1) 0)))
    (format #t "limit ~a, ll ~a, nn ~a~%" limit ll nn)
    (let iter ((i 1)
               (s 0))
;;      (if (zero? (remainder i 1000))
;;        (format #t "i ~a, s ~a~%" i s))
      (if (= i limit)
        s
        (if (let loop ((n i))
              (cond ((>= n nn)
                     (loop (number->square-of-digits n)))
                    ((= 1 (vector-ref tt n))
                     #f)
                    ((= 89 (vector-ref tt n))
                     #t)
                    ((= 1 n)
                     (vector-set! tt i 1)
                     #f)
                    ((= 89 n)
                     (vector-set! tt i 89)
                     #t)
                    (else
                      (if (loop (number->square-of-digits n))
                        (begin
                          (vector-set! tt n 89)
                          #t)
                        #f))))
          (iter (+ i 1) (+ s 1))
          (iter (+ i 1) s))))))



(define (p92)
  (p92- 10000000))



;; end of file
