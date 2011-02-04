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

;; end of file
;; vim: et ts=4 sw=4
