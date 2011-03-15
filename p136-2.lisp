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
;; progression. Given that n is a positive integer, the equation, 
;; x^2 - y^2 - z^2 = n, has exactly one solution when n = 20:
;;
;; 13^2 - 10^2 - 7^2 = 20
;;
;; In fact there are twenty-five values of n below one hundred for which the
;; equation has a unique solution.
;;
;; How many values of n less than fifty million have exactly one solution?
;;
;; Answer: 2544559


(defun p136 (&optional (limit 50000000))

  (let ((hits (make-array limit :element-type '(unsigned-byte 2))))

    (labels ((hit! (n) 
               (if (< (aref hits n) 2) 
                 (incf (aref hits n)))))

      (loop for y from 2 to (1- limit) do
            (loop for k from (1+ (truncate (/ y 4)))
                  to (min (1- y) 
                          (truncate (/ (+ (1- limit) (* y y)) (* 4 y))))
                  do (hit! (- (* 4 k y) (* y y)))))

      (loop for n from 0 to (1- limit)
            counting (= 1 (aref hits n)) into res
            finally (return res)))))


;; end of file
;; vim: sw=4 ts=4
