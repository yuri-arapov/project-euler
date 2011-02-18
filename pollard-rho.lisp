;; Pollard rho factoring.
;;
;; http://programmingpraxis.com/2011/01/21/pollard-rho-revisited/2/
;;
;; NOTE: unlike original (pollard factor n) the prime? predicate
;; NOTE: passed as argument.


(load "lisp-utils.lisp")


(defun wheel-factors (n limit)

  (defun wheel (&rest xs) 
    (nconc xs xs))

  (labels ((iter (n f fs ws)
             (cond ((< limit f) (values n (reverse fs)))
                   ((or (= n 1) (< n (* f f))) (values 1 (reverse (cons n fs))))
                   ((zerop (mod n f)) (iter (/ n f) f (cons f fs) ws))
                   (t (iter n (+ f (car ws)) fs (cdr ws))))))
    (iter n 2 '() (cons* 1 2 2 (wheel 4 2 4 2 4 6 2 6)))))




(defun pollard (primep-fn factor-fn n)
  (multiple-value-bind (n fs) (wheel-factors n 1000)
    (sort 
      (labels ((fact (n fs)
                 (cond ((= n 1) fs)
                       ((funcall primep-fn n) (cons n fs))
                       (t (let ((f (funcall factor-fn n 1 100000000)))
                            (append fs (fact f '()) (fact (/ n f) '())))))))
        (fact n fs))
      #'<)))




;;;(defun floyd (n c limit)
;;;  (defun f (x) (mod (+ (* x x) c) n))
;;;  (defun g (p t h) (mod (* p (abs (- t h))) n))
;;;  (let loop1 ((j 1) (t 2) (h (f 2)) (x 2) (y (f 2)) (p 1))
;;;    (if (= j limit) (error 'floyd "timeout")
;;;      (if (= t h) (floyd n (+ c 1) (- limit j))
;;;        (if (positive? (mod j 100)) (loop1 (+ j 1) (f t) (f (f h)) x y (g p t h))
;;;          (let ((d (gcd p n)))
;;;            (if (= d 1) (loop1 (+ j 1) (f t) (f (f h)) t h 1)
;;;              (if (< 1 d n) d
;;;                (let loop2 ((k 1) (x x) (y y) (d (gcd (- x y) n)))
;;;                  (if (= k 100) (floyd n (+ c 1) (- limit j))
;;;                    (if (= d 1) (loop2 (+ k 1) (f x) (f (f y)) (gcd (- x y) n))
;;;                      (if (= d n) (floyd n (+ c 1) (- limit j))
;;;                        d))))))))))))




(defun brent (n c limit)

  (defun f (y) (mod (+ (* y y) c) n))

  (defun g (p x y) (mod (* p (abs (- x y))) n))

  (labels ((loop1 (x y z j q p)
             (if (= j limit) (error 'brent "timeout")
               (if (= x y) (brent n (+ c 1) (- limit j)) ; cycle
                 (if (= j q) (let ((tt (f y))) (loop1 y (f y) z (+ j 1) (* q 2) (g p y tt)))
                   (if (positive? (mod j 100)) (loop1 x (f y) z (+ j 1) q (g p x y))
                     (let ((d (gcd p n)))
                       (if (= d 1) (loop1 x (f y) y (+ j 1) q (g p x y))
                         (if (< 1 d n) d ; factor

                           (labels ((loop2 (k z)
                                      (if (= k 100) (brent n (+ c 1) (- limit j))
                                        (let ((d (gcd (- z x) n)))
                                          (if (= d 1) (loop2 (+ k 1) (f z))
                                            (if (= d n) (brent n (+ c 1) (- limit j))
                                              d))))))
                             (loop2 1 (f z))))))))))))

    (loop1 2 (+ 4 c) (+ 4 c) 1 2 1)))


;; end of file
;; vim: ts=4 sw=4 et
