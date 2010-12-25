;; 2010-12-23
;;
;; Project Euler
;;
;; Problem 115
;; 24 February 2006
;;
;; NOTE: This is a more difficult version of problem 114.
;;
;; A row measuring n units in length has red blocks with a minimum length of m
;; units placed on it, such that any two red blocks (which are allowed to be
;; different lengths) are separated by at least one black square.
;;
;; Let the fill-count function, F(m, n), represent the number of ways that a
;; row can be filled.
;;
;; For example, F(3, 29) = 673135 and F(3, 30) = 1089155.
;;
;; That is, for m = 3, it can be seen that n = 30 is the smallest value for
;; which the fill-count function first exceeds one million.
;;
;; In the same way, for m = 10, it can be verified that F(10, 56) = 880711 and
;; F(10, 57) = 1148904, so n = 57 is the least value for which the fill-count
;; function first exceeds one million.
;;
;; For m = 50, find the least value of n for which the fill-count function
;; first exceeds one million.
;;
;; Answer: 168
;;  


(defun quotient (n d) (truncate (/ n d)))

(defun fill-count (m n)

  (let ((memo (make-array (1+ n) :initial-element nil)))

    (defun memo-ref (n)   (aref memo n))
    (defun memo-set (n x) (setf (aref memo n) x) x)

    (defun helper (n)
      (cond 
        ((< n m) 0)   ;; nothing
        ((= n m) 2)   ;; xxx and ooo
        (t
          (or (memo-ref n)
              (memo-set 
                n 
                (labels ((iter (left len acc)
                               (cond 
                                 ((> (+ left len) n) (iter 0 (1+ len) acc))
                                 ((= len n)          (+ 2 acc))
                                 (t
                                   (iter
                                     (1+ left)
                                     len
                                     (+ acc (max 1 (helper (- n (+ left len 1))))))))))
                  (iter 0 m 0)))))))
    (helper n)))


(defun upper-bound (m count)
  (labels ((iter (n)
                 (if (>= (fill-count m n) count)
                   n
                   (iter (* 2 n)))))
    (iter m)))


(defun p115-int (m count)
  (labels ((iter (n a b)
    (if (= 1 (- b a))
      (values b n)
      (let ((c (quotient (+ a b) 2)))
        (if (< (fill-count m c) count)
          (iter (1+ n) c b)
          (iter (1+ n) a c))))))
    (iter 0 m (upper-bound m count))))


(defun p115 ()
  (p115-int 50 1000000))

;; end of file
;; vim: ts=4 sw=4 et
