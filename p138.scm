;; 2011-03-14
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=138
;;
;; Problem 138
;; 20 January 2007
;;
;; Consider the isosceles triangle with base length, b = 16, and legs, L = 17.
;;
;;            .
;;           /|\
;;          / | \
;;         /  |  \
;;      L /  h|   \ L
;;       /    |    \
;;      /     |     \
;;     ---------------
;;            b
;;
;; By using the Pythagorean theorem it can be seen that the height of the
;; triangle, h = sqrt(17^2 - 8^2) = 15, which is one less than the base length.
;;
;; With b = 272 and L = 305, we get h = 273, which is one more than the base
;; length, and this is the second smallest isosceles triangle with the property
;; that h = b +/- 1.
;;
;; Find SUM(L) for the twelve smallest isosceles triangles for which h = b +/-
;; 1 and b, L are positive integers.
;;
;; Answer: 1118049290473932


(load "integer-square-root.scm")


;; square
(define (sqr x) (* x x))


;; Perfect square root: return either square root of n if n is perfect square
;; or #f otherwise.
(define (perfect-square-root n)
  (let ((r (integer-square-root n)))
    (and (= n (sqr r)) r)))



;; Make Pythagorean triple.
;; See http://en.wikipedia.org/wiki/Pythagorean_triple
;;
;; Note: m > n.
;;
;;    |\
;;  h | \ l
;;    |  \
;;    +---+
;;      b
(define (make-pt m n)
  (let ((b (- (sqr m) (sqr n)))
        (h (* 2 m n))
        (l (+ (sqr m) (sqr n))))
      (values b h l)))



;; Given m of Pythagorean triple generation formula compute n so that 
;; Pythagorean triple produced by (m,n) pair would meet this condition:
;; b = 2h +/- 1
(define (m->n m)
  (define (v->n v)
    (let ((r (perfect-square-root (+ (* 5 (sqr m)) v))))
      (and r (- r (* 2 m)))))
  (or (v->n -1) (v->n 1)))



;; Solve problem 138 iterating by m.
;; Way too slow.
(define (p138-m)
  (let loop ((m 2) (count 0) (S 0))
    (if (= 12 count) S
      (let ((n (m->n m)))
        (if n
          (begin
            (let-values (((b h l) (make-pt m n)))
              (format #t "~a: (~a ~a) ~a ~a~%" (1+ count)  m n (* 2 h) l)
              (loop (1+ m) (1+ count) (+ S l))))
          (loop (1+ m) count S))))))



;; Given m of Pythagorean triple generation formula compute n so that 
;; Pythagorean triple produced by (m,n) pair would meet this condition:
;; b = 2h +/- 1
(define (n->m n)
  (define (v->m v)
    (let ((r (perfect-square-root (+ (* 5 (sqr n)) v))))
      (and r (+ r (* 2 n)))))
  (or (v->m -1) (v->m 1)))



;; Solve problem 138 starting with smallest possible n.
;; When running p138-m I noticed that m(i)==n(i+1), see the pattern below:
;;
;;     m    n        B       L
;;     4    1       16      17
;;    17    4      272     305
;;    72   17     4896    5473
;;   305   72    87840   98209
;;  1292  305  1576240 1762289
;;
;; So I concluded that I can exploit this fact and use m from previos
;; iteration as n to compute next m.
(define (p138-n)
  (let loop ((n 1) (count 0) (S 0))
    (if (= 12 count) S
      (let ((m (n->m n)))
        (if (not m)
          (error "failed to compute m by n =" n))
        (let-values (((b h l) (make-pt m n)))
          (format #t "~2d: (~d ~d) ~d ~d~%" (1+ count) m n (* 2 h) l)
          (loop m (1+ count) (+ S l)))))))


;; end of file
;; vim: sw=4 ts=4
