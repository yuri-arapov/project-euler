;; 2011-11-11
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=144
;;
;; Problem 144
;; 09 March 2007
;;
;; In laser physics, a "white cell" is a mirror system that acts as a delay
;; line for the laser beam. The beam enters the cell, bounces around on the
;; mirrors, and eventually works its way back out.
;;
;; The specific white cell we will be considering is an ellipse with the
;; equation 4x^2 + y^2 = 100
;;
;; The section corresponding to -0.01<=x<=+0.01 at the top is missing, allowing
;; the light to enter and exit through the hole.
;;
;; The light beam in this problem starts at the point (0.0,10.1) just outside
;; the white cell, and the beam first impacts the mirror at (1.4,-9.6).
;;
;; Each time the laser beam hits the surface of the ellipse, it follows the
;; usual law of reflection "angle of incidence equals angle of reflection."
;; That is, both the incident and reflected beams make the same angle with the
;; normal line at the point of incidence.
;;
;; In the figure on the left, the red line shows the first two points of
;; contact between the laser beam and the wall of the white cell; the blue line
;; shows the line tangent to the ellipse at the point of incidence of the first
;; bounce.
;;
;; The slope m of the tangent line at any point (x,y) of the given ellipse is:
;; m = -4x/y
;;
;; The normal line is perpendicular to this tangent line at the point of
;; incidence.
;;
;; The animation on the right shows the first 10 reflections of the beam.
;;
;; How many times does the beam hit the internal surface of the white cell
;; before exiting?
;;
;; Answer: 354


(define (make-vec x y) (cons x y))

(define (x xy) (car xy))
(define (y xy) (cdr xy))

(define (vec-sub v1 v2) (make-vec (- (x v1) (x v2)) (- (y v1) (y v2))))
(define (vec-sum v1 v2) (make-vec (+ (x v1) (x v2)) (+ (y v1) (y v2))))
(define (vec-mul v mul) (make-vec (* (x v) mul) (* (y v) mul)))
(define (vec-rot+90 v)  (make-vec (- (y v)) (x v)))
(define (vec-rot-90 v)  (make-vec (y v) (- (x v))))

(define (vec-norm v)
  (define (sqr x) (* x x))
  (vec-mul v (/ 1 (sqrt (+ (sqr (x v)) (sqr (y v)))))))

(define (vec-dot-product v1 v2) (+ (* (x v1) (x v2)) (* (y v1) (y v2))))


;; v  |
;;  \ |
;;   \|
;;    +--->n
;;   /|
;;  / |
;; b  |
;;
;; compute vector b as a result of reflection of vector v on the surface with
;; given normal vector n.
(define (vec-bounce v n)
  (let* ((t (vec-rot+90 n))
         (rt (vec-dot-product t v))
         (rn (vec-dot-product n v)))
    (vec-sum
      (vec-mul t rt)
      (vec-mul n (- rn)))))


;; save as (vec-bounce ...) but the reflection surface is defined by tangent
;; vector t, NOT normal.
(define (vec-bounce-t v t)
  (vec-bounce v (vec-rot-90 t)))


;; compute intersection of the line defined as point P and tangent vector T and
;; ellipse (x/a)^2 + (y/b)^2 = 1.
;; just one point that corresponds to the larger parameter t.
(define (line-x-ellipse P T a b)
;; Maxima:
;;   solve(((Tx*t+Px)/a)^2 + ((Ty*t+Py)/b)^2=1,t);
;;
;;                 2     2    2                     2     2    2     2          2       
;; t = (a b sqrt((a  - Px ) Ty  + 2 Px Py Tx Ty + (b  - Py ) Tx ) - a  Py Ty - b  Px Tx)
;;   
;; /
;; 
;;   2   2    2   2 
;; (a  Ty  + b  Tx )
  (let* ((Tx (x T))
         (Ty (y T))
         (Px (x P))
         (Py (y P))
         (a2 (* a a))
         (b2 (* b b))
         (Px2 (* Px Px))
         (Py2 (* Py Py))
         (Tx2 (* Tx Tx))
         (Ty2 (* Ty Ty))
         (a2-Px2 (- a2 Px2))
         (b2-Py2 (- b2 Py2))
         (s (+ (* a2-Px2 Ty2) (* 2 Px Py Tx Ty) (* b2-Py2 Tx2)))
         (n (- (* a b (sqrt s)) (* a2 Py Ty) (* b2 Px Tx)))
         (d (+ (* a2 Ty2) (* b2 Tx2)))
         (t (/ n d)))
    (vec-sum P (vec-mul T t))))


;; same as (line-x-ellipse ...) but the ellipse is a circle.
(define (line-x-circle P T) (line-x-ellipse P T 1 1))


;; Solve problem 144
(define (p144)
  (let ((a 5)                   ; a of (x/a)^2 + (y/b)^2 = 1
        (b 10)                  ; b of above
        (s (make-vec 0 10.1))   ; starting point
        (r (make-vec 1.4 -9.6))); first reflection point
    (let loop ((count 0)
               (p s)
               (v (vec-norm (vec-sub r s))))
      (let ((i (line-x-ellipse p v a b)))
        (if (and (positive? (y i)) (< -0.01 (x i) 0.01))
          count
          (loop (1+ count) i
            (vec-bounce-t v (vec-norm (make-vec (y i) (* -4 (x i)))))))))))


;; end of file
;; vim: sw=4 ts=4
