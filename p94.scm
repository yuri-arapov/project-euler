;; 2010-05-09
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=94
;;
;; Problem 94
;; 29 April 2005
;;
;; It is easily proved that no equilateral triangle exists with integral length
;; sides and integral area. However, the almost equilateral triangle 5-5-6 has
;; an area of 12 square units.
;;
;; We shall define an almost equilateral triangle to be a triangle for which
;; two sides are equal and the third differs by no more than one unit.
;;
;; Find the sum of the perimeters of all almost equilateral triangles with
;; integral side lengths and area and whose perimeters do not exceed one
;; billion (1,000,000,000).
;;
;; Answer: 518408346
;;      


(define (f- x) (+ (* 3 x x) (* 2 x) -1)) ;; 3x^2 + 2x - 1
(define (f+ x) (- (* 3 x x) (* 2 x) +1)) ;; 3x^2 - 2x - 1


;; NOTE. x13 was discovered by comparing several matching triangles 
;;       in a row.  The ratio of sides of two consecutive triangles 
;;       was about 13.
;;
(define (next-n n)
  (let ((nn (* n 13)))
    (if (odd? nn)
      nn
      (1+ nn))))


;; Make list of solutions (solution is a triangle's side length)
;; for given magic function fn.
;;
(define (xx fn limit)
  (let loop ((n 3)
             (res '()))
    (if (> n limit)
      (reverse res)
      (let ((f (fn n)))
        (if (integer? (sqrt f))
          (loop (next-n n) (cons n res))
          (loop (+ n 2) res))))))


(define (div-check s)
  (let loop ((s s)
             (res '()))
    (if (null? (cdr s))
      (reverse res)
      (loop (cdr s) (cons (/ (cadr s) (car s) 1.) res)))))


(define (p94)
  (let ((s+ (xx f+ 333333333))  ;; triangles with bigger base side
        (s- (xx f- 333333333))) ;; triangles with smaller base side
    (let ((p+ (apply + (map (lambda (n) (1+ (* n 3))) s+)))
          (p- (apply + (map (lambda (n) (1- (* n 3))) s-))))
      (+ p+ p-))))


;; end of file
