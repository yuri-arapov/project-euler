;; 2010-12-23
;;
;; Project Euler
;;
;; Problem 114
;; 17 February 2006
;;
;; A row measuring seven units in length has red blocks with a minimum length
;; of three units placed on it, such that any two red blocks (which are allowed
;; to be different lengths) are separated by at least one black square. There
;; are exactly seventeen ways of doing this.
;;
;; ooooooo
;;
;; xxxoooo  xxxxooo  xxxxxoo  xxxxxxo  xxxxxxx
;; oxxxooo  oxxxxoo  oxxxxxo  oxxxxxx
;; ooxxxoo  ooxxxxo  ooxxxxx
;; oooxxxo  oooxxxx
;; ooooxxx
;;
;; xxxoxxx
;;
;; ('o' denotes black cell, 'x' denotes red one)
;;                                                      
;; How many ways can a row measuring fifty units in length be filled?
;;
;; NOTE: Although the example above does not lend itself to the possibility, in
;; general it is permitted to mix block sizes. For example, on a row measuring
;; eight units in length you could use red (3), black (1), and red (4).
;;
;; Answer: 16475640049
;;


(load "memo.lisp")


(defun p114-int (n)

  (let ((helper1 nil)
        (helper2 nil))

    (setf helper1
          (lambda (n) 
            (cond ((< n 3) 0)
                  ((= n 3) 2)
                  (t       (funcall helper2 n)))))

    (setf helper2 (make-memoized-proc
          (lambda (n)
            (labels ((iter (left len acc)
                           (cond 
                             ((> (+ left len) n) (iter 0 (1+ len) acc))
                             ((= len n)          (+ 2 acc))
                             (t
                               (iter
                                 (1+ left)
                                 len
                                 (+ acc (max 1 (funcall helper1 (- n (+ left len 1))))))))))
              (iter 0 3 0)))))

    (funcall helper1 n)))


(defun p114 () (p114-int 50))


;; end of file
;; vim: ts=4 sw=4 et
