;; The smallest positive integer n for which the numbers n^2+1, n^2+3, n^2+7, n^2+9,
;; n^2+13, and n^2+27 are consecutive primes is 10. The sum of all such integers n
;; below one-million is 1242490.
;; 
;; What is the sum of all such integers n below 150 million?
;; 
;; 
;; Answer: 676333270
;; 
;; some optimization stolen from here:
;; http://www.mathblog.dk/project-euler-146-investigating-a-prime-pattern/
;;


(load "miller-rabin-primality-test.scm")


(define *max-prime* 0)

(define (p? n)
  (if (prime? n)
    (begin
      (if (> n *max-prime*) (set! *max-prime* n))
      #t)
    #f))


(define (good? n)
  (let ((nn (* n n)))
    (and
      (and (= 1 (remainder nn 3))
           (or (= 2 (remainder nn 7)) 
               (= 3 (remainder nn 7))))
      (every (lambda (x) (positive? (remainder nn x)))
             '(9 13 27))
      (every (lambda (x) (p? (+ nn x))) 
             '(1 3 7 9 13 27))
      (every (lambda (x) (not (p? (+ nn x))))
             ;;'(11 17 19 21 23)))))
             '(19 21)))))


(define (p146-int limit)
  (let loop ((n 10)
             (acc 0))
    (if (zero? (remainder n 5000)) (format #t "~:d ~a (~a)\r" n acc *max-prime*))
    (if (>= n limit) 
      (begin
        (format #t "\n")
        acc)
      (loop (+ n 10)
            (if (good? n) (+ acc n)
              acc)))))


;; end of file
;; vim: ts=4 sw=4
