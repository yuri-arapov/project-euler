;; 2010-03-10
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=86
;;
;; Problem 86
;; 07 January 2005
;;
;; A spider, S, sits in one corner of a cuboid room, measuring 6 by 5 by 3, and
;; a fly, F, sits in the opposite corner. By travelling on the surfaces of the
;; room the shortest "straight line" distance from S to F is 10 and the path is
;; shown on the diagram.
;;
;; (Fig. here)
;;
;; However, there are up to three "shortest" path candidates for any given
;; cuboid and the shortest route is not always integer.
;;
;; By considering all cuboid rooms with integer dimensions, up to a maximum
;; size of M by M by M, there are exactly 2060 cuboids for which the shortest
;; distance is integer when M=100, and this is the least value of M for which
;; the number of solutions first exceeds two thousand; the number of solutions
;; is 1975 when M=99.
;;
;; Find the least value of M such that the number of solutions first exceeds one million.
;;
;; Answer: 1818


;; Square of the x.
;;
(define (sqr x) (* x x))


;; Square root.
;;
(define (sqrt x) (expt x 0.5))


;; True if d is divisor of the x.
;;
(define (divisor? d x)  (zero? (remainder x d)))


;; Compute possible sides of the right triangle for cuboid of LxWxH size.
;; For debugging purposes.
;;
(define (lwh l w h)
  (define (p a b) (+ (sqr a) (sqr b)))
  (map (lambda (x) (p (car x) (cdr x)))
       (list (cons l (+ w h))
             (cons w (+ h l))
             (cons h (+ l w)))))


;; Try to solve problem 86 limiting max cuboid size.
;; Used to be sure method is correct, for example:
;; (p86-int 100) gives (100 . 2060), and
;; (p86-int 99) gives (99 . 1975).
;;
(define (p86-int max-m)

  (define (test a b)
    (if (> a b)
      (test b a)
      (let*
        ((x (gcd a b))
         (a (/ a x))
         (b (/ b x)))
        (cond
          ((or (= 1 a) (= 1 b)) #f)

          ((even? (+ a b))      #f)
          ;; exactly one of a and b must be odd
          ;; (see Primitive Pythagorean triangles).

          (else
            (integer? (sqrt (+ (sqr a) (sqr b)))))))))

  (let loop ((l 2) (w 1) (h 1) (n 0))
    (cond
      ((> h w)
       (loop l (1+ w) w n))

      ((> w l)
       (cond
         ((> n 1000000) (cons l n))
         ((= l max-m)   (cons l n))
         (else
           (if (divisor? 100 l)
             (format #t "~a ~a\n" l n))
           (loop (1+ l) 1 1 n))))

      (else
        (if (test l (+ w h))
          (loop l w (1+ h)
                (if (<= (- (+ w h) l) 1)        ;; FIXME: comment
                  (+ n h)                       ;; FIXME: why n is increased
                  (- (+ n h) (- (+ w h) l 1)))) ;; FIXME: that ugly
          (loop l w (1+ h) n))))))


;; Solve problem 86.
;;
(define (p86) (p86-int 2000))


;; end of file
;; vim: sw=4 ts=4
