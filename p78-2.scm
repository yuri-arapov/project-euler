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
;; Answer: 55374
;;
;; See http://en.wikipedia.org/wiki/Partition_(number_theory)
;;
;; Way too slow...


;; Compute generalized pentagonal number
;; where n is 1, -1, 2, -2, 3, -3, etc.
;;
(define (gpn n)
  (* 1/2 n (- (* 3 n) 1)))


;; Compute index of generalized pentagonal number by "plain" index.
;;
;; (map gpn-index '(1 2 3 4 5 6 7 8)) ->
;;   (1 -1 2 -2 3 -3 4 -4)
;;
(define (gpn-index i) 
  (* (if (odd? i) 1 -1) (quotient (1+ i) 2)))


;; Compute sign (-1 or 1) of the i-th summand in the sequence:
;;
;; p = p1 + p2 - p3 - p4 + p5 + p6 - p7 - p8 + ...
;;
(define (sign i) (if (odd? (quotient (1+ i) 2)) 1 -1))


;; Memorization gizmo.
;;
(define  memo             (make-vector 1000000 0))
(define (memo-ref  n)     (vector-ref  memo n))
(define (memo-set! n val) (vector-set! memo n val) val)


;; Compute number of partitions of given k.
;;
(define (p k)
  (cond ((= 0 k) 1)
        ((= 1 k) 1)
        (else
          (if (positive? (memo-ref k))
            (memo-ref k)
            (let loop ((i 1) (res 0))
              (let ((n (gpn (gpn-index i))))
                (if (< k n)
                  (memo-set! k res)
                  (loop (1+ i) 
                        (+ res (* (sign i) (p (- k n))))))))))))


;; Solve problem 78.
;;
(define (p78)
  (let loop ((k 100))
    (let ((c (p k)))
      (if (zero? (remainder k 500))
        (format #t "~a ~a\n" k c))
      (if (zero? (remainder c 10000))
        (format #t "~a ~a\n" k c))
      (if (zero? (remainder c 1000000))
        k
        (loop (1+ k))))))


;; end of file
;; vim: ts=4 sw=4
