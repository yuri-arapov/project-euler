;; Various Lisp utils.
;;

;; Currying functions.
;; Found here:
;;   http://cl-cookbook.sourceforge.net/functions.html#curry
(declaim (ftype (function (function &rest t) function) curry)
         (inline curry))

(defun curry (function &rest args)
  (lambda (&rest more-args)
    (apply function (append args more-args))))


;; Scheme-to-List helper
(defun filter (proc list)
  (remove-if-not proc list))


;; Scheme-to-List helper
(defun for-each (proc list)
  (dolist (x list) (funcall proc x)))


;; Scheme-to-List helper
(defun fold (proc init list)
  (reduce #'(lambda (res i) (funcall proc i res))
          list
          :initial-value init))


;; Scheme-to-List helper
(defun fold-right (proc init list)
  (reduce #'(lambda (i res) (funcall proc i res))
          list
          :from-end t
          :initial-value init))


;; Range of numbers, both lo and hi included, lo <= hi.
(defun range (lo hi)
  (loop for n from lo to hi collect n))


;; Remove equal consecutive elements from the list.
(defun uniq (s &key (equal-proc #'=))
  (if (null s)
    s
    (nreverse 
      (reduce 
        #'(lambda (res e) 
            (if (funcall equal-proc (car res) e) 
              res 
              (cons e res)))
        s
        :initial-value (list (car s))))))


;; Same as Scheme's cons*.
(defun cons* (first &rest rest)
  (if (null rest) 
    first
    (cons first (apply #'cons* (car rest) (cdr rest)))))


(defun positive? (x) (> x 0))


;; end of file
;; vim: et ts=4 sw=4
