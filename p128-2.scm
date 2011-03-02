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


(define (A n)
  (list 
    (* 6 n)
    (+ (* 6 n) 1)
    1
    (* 6 (- n 1))
    (- (* 6 n) 1)
    (+ (* 12 n) 5)))


(define (B n)
  (list
    (* 6 n)
    (+ (* 6 n) 1)
    1
    (* 6 (- n 1))
    (- (* 6 n) 5)
    1))


(define (C n)
  (list
    (* 6 n)
    (+ (* 6 n) 1)
    (+ (* 6 n) 2)
    1
    (- (* 6 n) 5)
    1))


(define (D n)
  (list
    1
    (+ (* 6 n) 1)
    (+ (* 6 n) 2)
    1
    (- (* 6 n) 5)
    (- (* 6 n) 4)))


(define (E n)
  (list
    1
    (+ (* 6 n) 1)
    (+ (* 6 n) 2)
    (+ (* 6 n) 3)
    1
    (- (* 6 n) 4)))


(define (F n)
  (list
    (- (* 6 n) 3)
    1
    (+ (* 6 n) 2)
    (+ (* 6 n) 3)
    1
    (- (* 6 n) 2)))


(define (G n)
  (list
    (- (* 6 n) 3)
    1
    (+ (* 6 n) 2)
    (+ (* 6 n) 3)
    (+ (* 6 n) 4)
    1))


(define (H n)
  (list
    (- (* 6 n) 3)
    (- (* 6 n) 2)
    1
    (+ (* 6 n) 3)
    (+ (* 6 n) 4)
    1))


(define (I n)
  (list
    1
    (- (* 6 n) 2)
    1
    (+ (* 6 n) 3)
    (+ (* 6 n) 4)
    (+ (* 6 n) 5)))


(define (J n)
  (list
    1
    (- (* 6 n) 2)
    (- (* 6 n) 1)
    1
    (+ (* 6 n) 4)
    (+ (* 6 n) 5)))


(define (K n)
  (list
    (+ (* 6 n) 6)
    1
    (- (* 6 n) 1)
    1
    (+ (* 6 n) 4)
    (+ (* 6 n) 5)))


(define (L n)
  (list
    (+ (* 6 n) 6)
    1
    (- (* 6 n) 1)
    (* 6 n)
    1
    (+ (* 6 n) 5)))


(define (M n)
  (list
    (+ (* 6 n) 6)
    (- (* 6 n) 1)
    (- (* 12 n) 7)
    (* 6 n)
    1
    (+ (* 6 n) 5)))


(define (fold-range proc init from to)
  (let loop ((i from) (res init))
    (if (> i to)
      res
      (loop (1+ i) (proc i res)))))


(define (sort-uniq s)
  (fold (lambda (e res)
          (if (and (not (null? res)) (= e (car res)))
            res
            (cons e res)))
        '()
        (sort s <)))


(define (ni->val n i)
  (+ 2 i (* 6 (s (- n 1)))))


(define (p128-x limit)
  (call/cc 
    (lambda (return)
      (let loop ((n 2) (res 3))
        (loop 
          (1+ n)
          (fold
            (lambda (x res)
              (let ((f    (car x))
                    (from (cadr x))
                    (to   (caddr x)))
                (fold-range
                  (lambda (i res)
                    (if (= 3 (length (filter prime? (sort-uniq (f n)))))
                      (begin
                        (format #t "~a~%" (ni->val n i))
                        (if (= limit (1+ res))
                          (return (ni->val n i)))
                        (+ res 1))
                      res))
                  res
                  from 
                  to)))
            res
            (list
              (list A 0               0)
              (list B 1               (- n 1))
              (list C n               n)
              (list D (+ n 1)         (- (* n 2) 1))
              (list E (* n 2)         (* n 2))
              (list F (+ (* n 2) 1)   (- (* n 3) 1))
              (list G (* n 3)         (* n 3))
              (list H (+ (* n 3) 1)   (- (* n 4) 1))
              (list I (* n 4)         (* n 4))
              (list J (+ (* n 4) 1)   (- (* n 5) 1))
              (list K (* n 5)         (* n 5))
              (list L (+ (* n 5) 1)   (- (* n 6) 2))
              (list M (- (* n 6) 1)   (- (* n 6) 1)))))))))


;; end of file
;; vim: sw=4 ts=4
