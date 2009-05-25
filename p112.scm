;; 24 May 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=112
;;
;; Problem 112
;; 30 December 2005
;; 
;; Working from left-to-right if no digit is exceeded by the digit to its left
;; it is called an increasing number; for example, 134468.
;; 
;; Similarly if no digit is exceeded by the digit to its right it is called a
;; decreasing number; for example, 66420.
;; 
;; We shall call a positive integer that is neither increasing nor decreasing a
;; "bouncy" number; for example, 155349.
;; 
;; Clearly there cannot be any bouncy numbers below one-hundred, but just over
;; half of the numbers below one-thousand (525) are bouncy. In fact, the least
;; number for which the proportion of bouncy numbers first reaches 50% is 538.
;; 
;; Surprisingly, bouncy numbers become more and more common and by the time we
;; reach 21780 the proportion of bouncy numbers is equal to 90%.
;; 
;; Find the least number for which the proportion of bouncy numbers is exactly
;; 99%.
;; 
;; Answer: 1587000
;;
;; FIXME: This is bruteforce
;; FIXME: 70 seconds on AMD Athlon(tm) 64 X2 Dual Core Processor 3800+


;;
;; Turn number into list on 10-based digits.
;; Hihger digits first: 123 -> (3 2 1)
;;
(define (number->digits n)
  (if (< n 10)
    (list n)
    (cons (remainder n 10) (number->digits (quotient n 10)))))


;;
;; Turn list of digits into list of differences of
;; consecutive digits (right to lef): (1 2 0) -> (-1 2)
;;
(define (digits->diffs digits)
    (if (or (null? digits) (null? (cdr digits))) ;; less then 2 elements in the list
      '()
      (cons (- (first digits) (second digits)) 
            (digits->diffs (cdr digits)))))


;;
;; Test if this is bouncy number.
;;
(define (bouncy-number? n)
  (if (< n 100)
    #f ;; less then 3 digits: non-bouncy by definition.
    (let* ((a (number->digits n))                           ;; turn number into list of digits
           (b (digits->diffs a))
           (c (filter (lambda (d) (not (zero? d))) b))      ;; filter out zeros
           (d (map (lambda (d) (if (negative? d) -1 1)) c)));; replace diffs with -1 or 1
;;       (format #t "a: ~a\n" a)
;;       (format #t "b: ~a\n" b)
;;       (format #t "c: ~a\n" c)
;;       (format #t "d: ~a\n" d)
      (if (null? d)
        #f
        (not (every (lambda (s) (= (car d) s))  ;; make sure elements of 'd' are not the same
                    (cdr d)))))))


(define (p112)
  (let loop ((n 21780)          ;; numbers checked so far
             (b (* 9/10 21780)));; number of bouncy numbers in 0..n range
    (if (= (* b 100) (* n 99))
      n
      (loop (+ n 1) (if (bouncy-number? (+ n 1)) (+ b 1) b)))))


;; end of file
;; vim: ts=4 sw=4
