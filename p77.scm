;; 2010-02-26
;;
;; Project Euler
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=77
;;
;; Problem 77
;; 27 August 2004
;;
;; It is possible to write ten as the sum of primes in exactly five different
;; ways:
;;
;; 7 + 3
;; 5 + 5
;; 5 + 3 + 2
;; 3 + 3 + 2 + 2
;; 2 + 2 + 2 + 2 + 2
;;
;; What is the first value which can be written as the sum of primes in over
;; five thousand different ways?
;;
;; Answer: 71
;;
;; NOTE: memorization may speedup the computation.
;;
;; NOTE: this is recursive code, stack overflow is our friend here;
;;


(load "primes-1000000.scm")


;; Shortcut for list of primes under 1m.
;;
(define primes primes-1000000)


;; Return list of possible partitions of primes of given n.
;;
;; Example:
;; (num->partitions 10) -> 
;;   ((2 2 2 2 2) 
;;    (2 2 3 3) 
;;    (2 3 5) 
;;    (3 7) 
;;    (5 5))
;;
;; NOTE: if n is prime the will be partition containing just n:
;; (num->partitions 13) ->
;;   ((2 2 2 2 2 3) 
;;    ...
;;    (3 5 5) 
;;    (13))
;;
(define (num->partitions n)

  (define (iter n ps)
    (cond ((null? ps)
           '())

          ((> (car ps) n)
           '())

          ((= (car ps) n)
           (list (list (car ps))))

          (else
            (let ((rr (iter (- n (car ps)) ps)))
              (append
                (if (not (null? rr))
                  (map (lambda (i) (cons (car ps) i)) rr)
                  '())
                (iter n (cdr ps)))))))

  (iter n primes))


;; Solve problem 77.
;;
(define (p77)
  (let loop ((n 2))
    (let ((ll (length (num->partitions n))))
      (format #t "~a ~a\n" n ll)
      (if (> ll 5000)
        n
        (loop (1+ n))))))


;; end of file
;; vim: ts=4 sw=4
