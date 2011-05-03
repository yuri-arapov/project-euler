;; 2011-04-23
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=139
;;
;; Problem 139
;; 27 January 2007
;;
;; Let (a, b, c) represent the three sides of a right angle triangle with
;; integral length sides. It is possible to place four such triangles together
;; to form a square with length c.
;;
;; For example, (3, 4, 5) triangles can be placed together to form a 5 by 5
;; square with a 1 by 1 hole in the middle and it can be seen that the 5 by 5
;; square can be tiled with twenty-five 1 by 1 squares.
;;
;; However, if (5, 12, 13) triangles were used then the hole would measure 7 by
;; 7 and these could not be used to tile the 13 by 13 square.
;;
;; Given that the perimeter of the right triangle is less than one-hundred
;; million, how many Pythagorean triangles would allow such a tiling to take
;; place?
;;
;; Answer: 10057761


(load "memo.scm")   ; for Pell numbers sequence


;; Square of x.
(define (sqr x) (* x x))


;; Area of the hole made by 4 a,b,c Pythagorean triangles.
(define (hole-area a b c) (- (* c c) (* 2 a b)))


;; Make Pythagorean triple.
;; See http://en.wikipedia.org/wiki/Pythagorean_triple
;; m > n.
(define (make-pt m n)
  (let ((a (- (sqr m) (sqr n)))
        (b (* 2 m n))
        (c (+ (sqr m) (sqr n))))
      (values a b c)))


;; Generate series of Pythagorean triangles and see if they match
;; conditions of the problem.
(define (test max-m)
  (dotimes (m 2 max-m)
    (dotimes (n 1 (1- m))
      (let-values (((a b c) (make-pt m n)))
        (let ((hole (hole-area a b c))
              (s    (* c c)))
          (format #t "~4d ~4d   ~6d ~6d ~6d   ~8d ~8d  ~a~a~%" 
                  m n a b c (hole-area a b c) (* c c) (/ s hole)
                  (if (= 1 (abs (- a b))) " *" "")
                  ))))))


;; Compute n-th Pell number.
;; n is 0-based.
(define p (make-memoized-proc =
  (lambda (n)
    (case n
      ((0) 0)
      ((1) 1)
      (else (+ (* 2 (p (- n 1))) (p (- n 2))))))))


;; Compute n-th primitive Pythagorean triple.
;; n is 1-based.
;; http://rationalargumentator.com/issue232/Pellian.pdf
(define (ppt n)
  (make-pt (p (1+ n)) (p n)))


;; Solve problem 139.
;; While playing with (test ...) it was noticed that any primitive
;; Pythagorean triangle matching condition of the problem was matching yet
;; another condition: |a - b| = 1, ie difference of its lengs was one.
;; Web search for Pythagorean triples generation gave another fact: Pell numbers
;; used for generating Pythagorean triples provide such a quality of triangles
;; (see http://rationalargumentator.com/issue232/Pellian.pdf for more details).
(define (p139-ex limit)
  (let loop ((n 1) (res 0))
    (let-values (((a b c) (ppt n)))
      (let ((p (+ a b c))) ; perimeter
        (if (>= p limit)
          res
          (loop (1+ n) (+ res (quotient (1- limit) p))))))))
          ;                   ~~~~~~~~~~~~~~~~~~~~~~~
          ;                   This is how many Pythagorean
          ;                   triangles derived from given primitive
          ;                   Pythagorean triangle will stay under
          ;                   the given limit.


(define (p139)
  (p139-ex 100000000))



;; end of file
;; vim: sw=4 ts=4
