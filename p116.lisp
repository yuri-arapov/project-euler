;; 2010-12-27
;;
;; Project Euler
;;
;; Problem 116
;; 03 March 2006
;;
;; A row of five black square tiles is to have a number of its tiles replaced
;; with coloured oblong tiles chosen from red (length two), green (length
;; three), or blue (length four).
;;
;; If red tiles are chosen there are exactly seven ways this can be done.
;;
;;   xxooo oxxoo ooxxo oooxx
;;   xxxxo xxoxx oxxxx
;;
;; If green tiles are chosen there are three ways.
;;
;;   xxxoo oxxxo ooxxx
;;
;; And if blue tiles are chosen there are two ways.
;;
;;   xxxxo oxxxx
;;
;; Assuming that colours cannot be mixed there are 7 + 3 + 2 = 12 ways of
;; replacing the black tiles in a row measuring five units in length.
;;
;; How many different ways can the black tiles in a row measuring fifty units
;; in length be replaced if colours cannot be mixed and at least one coloured
;; tile must be used?
;;
;; NOTE: This is related to problem 117.
;;
;; Answer: 20492570929


(defun fill-count (color-len black-len)

  (let ((memo (make-array (1+ black-len) :initial-element nil)))

    (defun memo-ref  (len)   (aref memo len))
    (defun memo-set! (len x) (setf (aref memo len) x) x)

    (defun helper (len)
      (or (memo-ref len)
          (memo-set! 
            len
            (labels ((iter (left acc)
                           (if (> (+ left color-len) len)
                             acc
                             (let ((x (helper (- len (+ left color-len)))))
                               (iter (1+ left) (+ acc (if (zerop x) 1 (1+ x))))))))
              (iter 0 0)))))

    (helper black-len)))


(defun p116 ()
  (apply #'+ (mapcar #'(lambda (c) (fill-count c 50)) '(2 3 4))))


;; end of file
;; vim: ts=4 sw=4 et
