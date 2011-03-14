;; 2011-03-13
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=136
;;
;; Problem 136
;; 29 December 2006
;;
;; The positive integers, x, y, and z, are consecutive terms of an arithmetic
;; progression. Given that n is a positive integer, the equation, x2  y2  z2 =
;; n, has exactly one solution when n = 20:
;;
;; 132  102  72 = 20
;;
;; In fact there are twenty-five values of n below one hundred for which the
;; equation has a unique solution.
;;
;; How many values of n less than fifty million have exactly one solution?
;;
;; Answer: 2544559


(defun make-bitvector (n)       (make-array n :element-type '(unsigned-byte 1)))
(defun bitvector-ref  (bv i)    (not (zerop (aref bv i))))
(defun bitvector-set! (bv i val)(setf (aref bv i) (if val 1 0)))

(defun quotient (n d) (truncate (/ n d)))


(defun p136-ex (limit)

  (let ((hits (make-bitvector limit))
        (full (make-bitvector limit)))

    (defun hit?  (n) (bitvector-ref hits n))
    (defun full? (n) (bitvector-ref full n))

    (defun hit! (n)
      (if (hit? n)
        (bitvector-set! full n t)
        (bitvector-set! hits n t)))

    (loop for y from 2 to (1- limit) do

      (progn

        (if (zerop (rem y 10000))
          (format t "~a~%" y))

        (loop for k from (1+ (quotient y 4))
                    to (min (1- y) (quotient (+ (1- limit) (* y y)) (* 4 y)))
           do (hit! (- (* 4 k y) (* y y))))))

    (loop for n from 0 to (1- limit)
          counting (and (not (full? n)) (hit? n)) into res
          finally (return res))))


(defun p136 ()
  (p136-ex 50000000))


;; end of file
;; vim: sw=4 ts=4
