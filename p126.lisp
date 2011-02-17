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
;; We shall defun C(n) to represent the number of cuboids that contain n cubes
;; in one of its layers. So C(22) = 2, C(46) = 4, C(78) = 5, and C(118) = 8.
;;
;; It turns out that 154 is the least value of n for which C(n) = 10.
;;
;; Find the least value of n for which C(n) = 1000.
;;
;; Answer: 18522


(load "lisp-utils")


;; Compute sum 1+2+3+...+n
(defun s (n) 
  (if (< n 0) 
    0
    (* (1+ n) n 1/2)))


;; Compute number of blocks of n-th layer of LxWxH cuboid.
(defun nth-layer (l w h n)
  (+ (* 2 (+ (* l w) (* w h) (* h l)))
     (* 4 (- n 1) (+ l w h))
     (* 8 (s (- n 2)))))


;; Determine smallest number of blocks -- ie smallest area, up to max-area --
;; so that (count-cuboids area) is exactly given number of cuboids.
;; Return #f if failed.
(defun min-layer-area (cuboids max-area)

  (let ((cuboids-by-area (make-array (1+ max-area) :initial-element 0)))

    (labels ((inc-cuboids! (area) 
                           (and (<= area max-area)
                                (incf (elt cuboids-by-area area)))))

      (do ((x 1 (1+ x))) ((> (nth-layer x 1 1 1) max-area))
        (do ((y 1 (1+ y))) ((or (> y x) (> (nth-layer x y 1 1) max-area)))
          (do ((z 1 (1+ z))) ((or (> z y) (> (nth-layer x y z 1) max-area)))
            (do ((n 1 (1+ n))) ((not (inc-cuboids! (nth-layer x y z n))))))))

      (position-if (curry #'= cuboids) cuboids-by-area))))


;; Problem 126, custom parameter.
(defun p126-ex (cuboids)
  (labels ((iter (max-area)
             (or (min-layer-area cuboids max-area)
                 (iter (* 2 max-area)))))
    (iter 300)))


;; Problem 126.
(defun p126 ()
  (p126-ex 1000))


;; end of file
;; vim: sw=3 ts=4
