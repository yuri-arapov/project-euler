;; 2010-02-27
;;
;; Project Euler
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=78
;;
;; Problem 78
;; 10 September 2004
;;
;; Let p(n) represent the number of different ways in which n coins can be
;; separated into piles. For example, five coins can separated into piles in
;; exactly seven different ways, so p(5)=7.
;;
;; ooooo
;; oooo o
;; ooo  oo
;; ooo  o   o
;; oo   oo  o
;; oo   o   o   o
;; o    o   o   o   o
;;
;; Find the least value of n for which p(n) is divisible by one million.
;;
;; Answer: ???


;; Return list of possible partitions of descending numbers of given n.
;;
;; Example:
;; (num->partitions 5) -> 
;;  FIXME: 
;;
(define (num->partitions n)

  (define (iter n m)
    (cond ((> m n)
           '())

          ((= m n)
           (list (list m)))

          (else
            (let ((rr (iter (- n m) m)))
              (append
                (if (not (null? rr))
                  (map (lambda (i) (cons m i)) rr)
                  '())
                (iter n (1+ m)))))))

  (iter n 1))


(define (make-table rows cols default)
  (list->vector
    (map (lambda (x) (make-vector cols default)) 
         (make-list rows))))

(define (table-ref  tab row col)     (vector-ref  (vector-ref tab row) col))
(define (table-set! tab row col val) (vector-set! (vector-ref tab row) col val) val)


(define  memo               (make-table 1000 1000 0))
(define (memo-ref  n m)     (table-ref  memo n m))
(define (memo-set! n m val) (table-set! memo n m val))


(define (num->part-count n)

  (define (iter n m res)
    (cond ((> m n) res)

          ((= m n) (1+ res))

          (else 
            (iter n (1+ m) (iter (- n m) m res)))))

  (iter n 1 0))


;; Solve problem 77.
;;
(define (p77)
  (let loop ((n 2))
    (let ((c (num->part-count n)))
      (if (zero? (remainder c 1000000))
        n
        (loop (1+ n))))))


;; end of file
;; vim: ts=4 sw=4
