;; Memoization


(defun make-memoized-proc (proc)
  (let ((proc proc)
        (ht   (make-hash-table)))
    (lambda (key)
      (let ((v (gethash key ht)))
        (if v
          v
          (let ((v (funcall proc key)))
            (setf (gethash key ht) v)
            v))))))


;; end of file
;; vim: ts=4 sw=4 et
