;; 2011-02-13
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=126
;;
;; Problem 126
;; 18 August 2006
;;
;; The minimum number of cubes to cover every visible face on a cuboid
;; measuring 3 x 2 x 1 is twenty-two.
;;
;; If we then add a second layer to this solid it would require forty-six cubes
;; to cover every visible face, the third layer would require seventy-eight
;; cubes, and the fourth layer would require one-hundred and eighteen cubes to
;; cover every visible face.
;;
;; However, the first layer on a cuboid measuring 5 x 1 x 1 also requires
;; twenty-two cubes; similarly the first layer on cuboids measuring 5 x 3 x 1,
;; 7 x 2 x 1, and 11 x 1 x 1 all contain forty-six cubes.
;;
;; We shall define C(n) to represent the number of cuboids that contain n cubes
;; in one of its layers. So C(22) = 2, C(46) = 4, C(78) = 5, and C(118) = 8.
;;
;; It turns out that 154 is the least value of n for which C(n) = 10.
;;
;; Find the least value of n for which C(n) = 1000.
;;
;; Answer: 18522


(load "quadratic-equation.scm")


;; Compute sum 1+2+3+...+n
(define (s n) 
  (if (negative? n) 
    0
    (* (1+ n) n 1/2)))


;; Compute number of blocks of n-th layer of LxWxH cuboid.
(define (nth-layer l w h n)
  (+ (* 2 (+ (* l w) (* w h) (* h l)))
     (* 4 (- n 1) (+ l w h))
     (* 8 (s (- n 2)))))


;; Determine number of the layer of LxWxH cuboid that contains 'area' 
;; blocks.
;; Return #f in no such layer.
;; NOTE: quadratic equation coeffs. are determined from formulae used in
;; NOTE: (nth-layer ...).
(define (layer-by-area l w h area)
  (let-values (((n1 n2) (quadratic-equation 
                          4
                          (* 4 (+ l w h -1))
                          (- (* 2 (+ (* l w) (* w h) (* h l))) area))))
    (let ((n (any (lambda (n) (and n (>= n 0) (integer? n) (inexact->exact n)))
                  (list n1 n2))))
      (and n (1+ n)))))


;; Simple accumulator.
(define (accum init next test incr)
  (let loop ((i init)
             (res 0))
    (if (not (test i))
      res
      (loop (next i) (+ res (incr i))))))


;; Count cuboids that contain 'area' blocks on some of their layers.
;; For debugging purposes.
(define (count-cuboids area)
  (accum 1 1+ (lambda (x) (<= (nth-layer x 1 1 1) area))
    (lambda (x)
      (accum 1 1+ (lambda (y) (and (<= y x) (<= (nth-layer x y 1 1) area)))
        (lambda (y) 
          (accum 1 1+ (lambda (z) (and (<= z y) (<= (nth-layer x y z 1) area)))
            (lambda (z)
              (if (layer-by-area x y z area) 1 0))))))))


;; Determine smallest number of blocks -- ie smallest area, up to max-area --
;; so that (count-cuboids area) is exactly given number of cuboids.
;; Return #f if failed.
(define (min-layer-area cuboids max-area)

  (let ((cuboids-by-area (make-vector (1+ max-area) 0)))

    (define (inc-cuboids! area) 
      (and (<= area max-area)
           (begin 
             (vector-set! cuboids-by-area area (1+ (vector-ref cuboids-by-area area)))
             #t)))

    (do ((x 1 (1+ x))) ((> (nth-layer x 1 1 1) max-area))
      (do ((y 1 (1+ y))) ((or (> y x) (> (nth-layer x y 1 1) max-area)))
        (do ((z 1 (1+ z))) ((or (> z y) (> (nth-layer x y z 1) max-area)))
          (do ((n 1 (1+ n))) ((not (inc-cuboids! (nth-layer x y z n))))))))

    (list-index (curry = cuboids) (vector->list cuboids-by-area))))


;; Problem 126, custom parameter.
(define (p126-ex cuboids)
  (let loop ((max-area 300))
    (format #t "~a~%" max-area)
    (or (time (min-layer-area cuboids max-area))
        (loop (* 2 max-area)))))


;; Problem 126.
(define (p126)
  (p126-ex 1000))


;; end of file
;; vim: sw=3 ts=4
