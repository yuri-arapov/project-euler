;; 2011-02-27
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=128
;;
;; Problem 128
;; 29 September 2006
;;
;; A hexagonal tile with number 1 is surrounded by a ring of six hexagonal
;; tiles, starting at "12 o'clock" and numbering the tiles 2 to 7 in an
;; anti-clockwise direction.
;;
;; New rings are added in the same fashion, with the next rings being numbered
;; 8 to 19, 20 to 37, 38 to 61, and so on. The diagram below shows the first
;; three rings.
;;
;;
;;                     (picture here)
;;
;;
;; By finding the difference between tile n and each its six neighbours we
;; shall define PD(n) to be the number of those differences which are prime.
;;
;; For example, working clockwise around tile 8 the differences are 12, 29, 11,
;; 6, 1, and 13. So PD(8) = 3.
;;
;; In the same way, the differences around tile 17 are 1, 17, 16, 1, 11, and
;; 10, hence PD(17) = 2.
;;
;; It can be shown that the maximum value of PD(n) is 3.
;;
;; If all of the tiles for which PD(n) = 3 are listed in ascending order to
;; form a sequence, the 10th tile would be 271.
;;
;; Find the 2000th tile in this sequence.
;;
;; Answer: 


(load "miller-rabin-primality-test.scm")


(define (s n) (* (+ n 1) (/ n 2)))


(define (ni->xyz n i)
  (let ((s (quotient i n))
        (o (remainder i n)))
    (case s
      ((0) (list n       o       0))
      ((1) (list (- n o) n       0))
      ((2) (list 0       n       o))
      ((3) (list 0       (- n o) n))
      ((4) (list o       0       n))
      ((5) (list n       0       (- n o)))
      (else
        (error "ni->xyz: bad arguments:" n i)))))


(define (xyz->ni x y z)
  (if (and (zero? x) (zero? y) (zero? z))
    (values 0 0)
    (let ((n (max x y z)))
      (let ((i (cond ((and (= x n)          (<= 0 y (- n 1)) (= z 0))          y)
                     ((and (<= 1 x n)       (= y n)          (= z 0))          (- (* n 2) x))
                     ((and (= x 0)          (= y n)          (<= 0 z (- n 1))) (+ (* n 2) z))
                     ((and (= x 0)          (<= 1 y n)       (= z n))          (- (* n 4) y))
                     ((and (<= 0 x (- n 1)) (= y 0)          (= z n))          (+ (* n 4) x))
                     ((and (= x n)          (= y 0)          (<= 1 z n))       (- (* n 6) z))
                     (else
                       (error "xyz->ni: bad arguments:" x y z)))))
        (values n i)))))


(define (test-ni-xyz n i)
  (let-values (((nn ii) (apply xyz->ni (ni->xyz n i))))
    (and (= n nn) (= i ii))))


(define (ni->val n i)
  (if (zero? n)
    1
    (+ 2 i (* 6 (s (1- n))))))


(define (xyz->val xyz)
  (let-values (((n i) (apply xyz->ni xyz)))
    (ni->val n i)))


(define (fix-xyz xyz)
  (map (rcurry - (apply min xyz)) xyz))


(define (neighbours xyz)
  (let-values (((x y z) (apply values xyz)))
    (map fix-xyz
         (list (list (1- x) y      z)
               (list (1+ x) y      z)
               (list x      (1- y) z)
               (list x      (1+ y) z)
               (list x      y     (1- z))
               (list x      y     (1+ z))))))


(define (diff a b) (abs (- a b)))


(define (show x)
  (format #t "~a~%" x)
  x)


(define (neighbours-diff xyz)
  (show
    (map (curry diff (xyz->val xyz))
         (map xyz->val (neighbours xyz)))))


(define (sort-uniq s)
  (fold (lambda (e res)
          (if (and (not (null? res)) (= e (car res)))
            res
            (cons e res)))
        '()
        (sort s <)))


(define (bingo? xyz)
  (= 3 (length (filter prime? (sort-uniq (neighbours-diff xyz))))))


(define (fold-enum proc init from end?)
  (let loop ((n from) (res init))
    (if (end? n)
      res
      (loop (1+ n) (proc n res)))))


(define (p128-ex limit)
  (let loop ((n 1) (i 0) (count 1))
    (if (= i (* 6 n))
      (loop (1+ n) 0 count)
      (let ((xyz (ni->xyz n i)))
        (if (bingo? xyz)
          (begin
            (format #t "~a ~a: ~a ~a~%" n i (xyz->val xyz) (1+ count))
            (if (= (1+ count) limit)
              (xyz->val xyz)
              (loop n (1+ i) (1+ count))))
          (loop n (1+ i) count))))))

;; end of file
;; vim: sw=4 ts=4
