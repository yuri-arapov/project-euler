;; 2011-03-15
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=135
;;
;; Problem 135
;; 29 December 2006
;;
;; Given the positive integers, x, y, and z, are consecutive terms of an
;; arithmetic progression, the least value of the positive integer, n, for
;; which the equation, x^2 - y^2 - z^2 = n, has exactly two solutions is n = 27:
;;
;; 34^2 - 27^2 - 20^2 = 12^2 - 9^2 - 6^2 = 27
;;
;; It turns out that n = 1155 is the least value which has exactly ten
;; solutions.
;;
;; How many values of n less than one million have exactly ten distinct
;; solutions?
;;
;; Answer: 4989


(defun p135 (&optional (limit 1000000))

  (let ((hits (make-array limit :element-type '(unsigned-byte 5))))

    (labels ((hit! (n) 
               (if (< (aref hits n) 11) 
                 (incf (aref hits n)))))

      (loop for y from 2 to (1- limit) do
        (loop for k from (1+ (truncate (/ y 4)))
                    to (min (1- y) 
                            (truncate (/ (+ (1- limit) (* y y)) (* 4 y))))
              do (hit! (- (* 4 k y) (* y y)))))

      (loop for n from 0 to (1- limit)
            counting (= 10 (aref hits n)) into res
            finally (return res)))))


;; end of file
;; vim: sw=4 ts=4
