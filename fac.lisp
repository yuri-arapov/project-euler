;; fac.lisp
;;
;; Factorial


(defun fac (n)
  (loop for i from 1 to n
        for s = 1 then (* s i)
        finally (return s)))


;; end of file
;; vim: set ts=4 sw=4 et:
