;; each-to-each
;; Apply operation op for each pair of elements from s1 and s2,
;; return list of resultant values
;;
(defun each-to-each (op s1 s2)
  (reverse
    (reduce #'(lambda (res1 i1)
                (reduce #'(lambda (res2 i2) (cons (funcall op i1 i2) res2)) 
                        s2
                        :initial-value res1))
            s1
            :initial-value '())))


;; TODO: for-each-to-each
;; TODO: map-each-to-each
;; TODO: fold-each-to-each


;; Join elements of the lists to each other:
;; (product '(a b) '(c d e)) ->
;; ((a c) (a d) (a e) (b c) (b d) (b e))
;;
(defun product (s1 s2)
  (defun tolist (i) (if (listp i) i (list i)))
  (each-to-each #'(lambda (i1 i2) (append (tolist i1) (tolist i2))) s1 s2))

;; end of file
