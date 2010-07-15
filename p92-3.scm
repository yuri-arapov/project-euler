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
;; A number chain is created by continuously adding the square of the digits in
;; a number to form a new number until it has been seen before.
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
;; Answer: 8581146
;;
;; FIXME: bruteforce


;; Return number of decimal digits of number n
;;
(define (number-of-digits n)
  (let loop ((n n)
             (r 1))
    (if (< n 10)
      r
      (loop (quotient n 10) (1+ r)))))


;; Increment number represented as a list of decimal digits
;; (lower digits first).
;;
(define (inc digits)
  (let loop ((head '())
             (pos  (car digits))
             (tail (cdr digits)))
    (cond ((< pos 9)
           (append head (list (1+ pos)) tail))
          ((null? tail)
           (append head (list 0 1))) ;; auto expand
          (else
            (loop (append head (list 0)) (car tail) (cdr tail))))))


;; Make a number represented as list of digits of given length.
;;
(define (make-sd-num max-digits)
  (define (make num digits)
    (let ((number num)
          (digits digits))
      (let ((dispatch 
              (lambda (op)
                (cond ((eq? op 'get-number)
                       number)

                      ((eq? op 'get-squared)
                       (apply + (map (lambda (x) (* x x)) digits)))

                      ((eq? op 'get-max-squared)
                       (apply + (map (lambda (x) (* 9 9)) digits)))

                      ((eq? op 'next)
                       (make (1+ num) (inc digits)))

                      (else
                        (error "bad operation:" op))))))
        dispatch)))
  (make 0 (make-list max-digits 0)))


(define (xxx limit)
  (let loop ((n (make-sd-num limit)))
    (let ((x (n 'get-max-squared))
          (y (n 'get-number)))
      (if (zero? (remainder y 1000))
        (format #t "~a\n" y))
      (if (> (n 'get-number) limit)
        (n 'get-max-squared)
        (loop (n 'next))))))

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
  (let* ((max-digits      (number-of-digits limit))
         (num             (make-sd-num max-digits))
         (max-squared-sum (num 'get-max-squared))
         (tt              (make-vector (1+ max-squared-sum) 0)))

    (define (mark n ls)
      (for-each (lambda (i) (vector-set! tt i n)) ls))

    (define (test n)
      (let loop ((n n) 
                 (candidates '()))
        (cond ((> n max-squared-sum)
               (= 89 (vector-ref tt (number->square-of-digits n))))
              ((or (= 1 (vector-ref tt n))
                   (= 1 n))
               (mark 1 (cons n candidates))
               #f)
              ((or (= 89 (vector-ref tt n))
                   (= 89 n))
               (mark 89 (cons n candidates))
               #t)
              (else
                (loop (number->square-of-digits n) (cons n candidates))))))

    (let iter ((i 1) 
               (s 0))
      (if (zero? (remainder i 10000))
        (format #t "~a ~a\n" i s))
      (if (= i limit)
        (begin
          (format #t "~a\n" tt)
          s)
        (iter (1+ i) (if (test i) (1+ s) s))))))


(define (p92)
  (p92- 10000000))



;; end of file
