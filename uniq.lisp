;; uniq.lisp


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


;; end of file
