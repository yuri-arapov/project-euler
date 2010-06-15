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


;; Return sum of squared digits of given number
;;
(define (number->square-of-digits n)
  (let loop ((n n)
             (s 0))
    (if (zero? n)
      s  
      (loop (quotient n 10) 
            (+ s ((lambda (x) (* x x)) (remainder n 10)))))))


;; Increment value conditionally
;;
(define (cond-inc c val) (if c (1+ val) val))


;; Problem 92 (user-defined limit)
;;
(define (p92- limit)

  (let* ((max-digits      (number-of-digits limit))
         (max-squared-sum (apply + (make-list max-digits (* 9 9))))
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
        (iter (1+ i) (cond-inc (test i) s))))))


;; Problem 92
;;
(define (p92)
  (p92- 10000000))



;; end of file
