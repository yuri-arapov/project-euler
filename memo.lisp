;; Memoization


(defun make-memoized-proc (proc)
  (let ((proc proc)
        (ht   (make-hash-table)))
    (lambda (key)
      (or (gethash key ht)
          (setf (gethash key ht) (funcall proc key))))))


;; end of file
;; vim: ts=4 sw=4 et
