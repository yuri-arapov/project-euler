;; 2010-08-15
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=101
;;
;; Problem 102
;; 12 August 2005
;;
;; Three distinct points are plotted at random on a Cartesian plane, for which
;; -1000 ≤ x, y ≤ 1000, such that a triangle is formed.
;;
;; Consider the following two triangles:
;;
;; A(-340,495), B(-153,-910), C(835,-947)
;;
;; X(-175,41), Y(-421,-714), Z(574,-645)
;;
;; It can be verified that triangle ABC contains the origin, whereas triangle
;; XYZ does not.
;;
;; Using triangles.txt (right click and 'Save Link/Target As...'), a 27K text
;; file containing the co-ordinates of one thousand "random" triangles, find
;; the number of triangles for which the interior contains the origin.
;;
;; NOTE: The first two examples in the file represent the triangles in the
;; example given above.
;;
;; Answer: 228



(load "read-file.scm")
(load "group-by.scm")



;; Vector stuff.
;;
(define (make-v x y) (cons x y))
(define (x v) (car v))
(define (y v) (cdr v))
(define (v- v1 v2) (make-v (- (x v1) (x v2)) (- (y v1) (y v2))))
(define (v+ v1 v2) (make-v (+ (x v1) (x v2)) (+ (y v1) (y v2))))



;; Determinant.
;;
(define (det v1 v2)
  (- (* (x v1) (y v2)) 
     (* (y v1) (x v2))))


;; Determine if point p belongs to interior of triangle (p1,p2,p3).
;; See http://mathworld.wolfram.com/TriangleInterior.html
;;
(define (point-inside-triangle? p1 p2 p3 p)
  (let ((v0 p1)
        (v1 (v- p2 p1))
        (v2 (v- p3 p1))
        (v  p))
    (let ((v1v2 (det v1 v2)))
      (if (zero? v1v2)
        #f
        (let ((v0v1 (det v0 v1))
              (v0v2 (det v0 v2))
              (vv1  (det v v1))
              (vv2  (det v v2)))
          (let ((a (/ (- vv2 v0v2) v1v2))
                (b (- (/ (- vv1 v0v1) v1v2))))
            (and (positive? a)
                 (positive? b)
                 (< (+ a b) 1))))))))



(define (test)
  (point-inside-triangle? (make-v -340  495) 
                          (make-v -153 -910) 
                          (make-v  835 -947) 
                          (make-v    0    0)))

(define (test1)
  (point-inside-triangle? (make-v   0  10)
                          (make-v  10 -10)
                          (make-v -10 -10)
                          (make-v   0   0)))



;; Problem 101
;;
(define (p101)
  (count
    (lambda (i) i)  ;; second argument of (count ...) is a list containg #t/#f.
    (read-file-with 
      "triangles.txt"
      (lambda (line)
        (let-values ((
            (p1 p2 p3)
            (apply values
                   (map (lambda (e) 
                          (make-v (car e) (cadr e)))
                        (group-by 
                          2 
                          (map string->number 
                               (string-split line #\,)))))))
          (point-inside-triangle? p1 p2 p3 (make-v 0 0)))))))



;; end of file
;; vim: ts=4 sw=4
