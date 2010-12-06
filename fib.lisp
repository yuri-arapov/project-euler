;; Fibonacci numbers

(defun fib (n)
  (defun iter (i a b)
    (if (= i n)
      a
      (iter (+ 1 i) b (+ a b))))
  (iter 0 0 1))


(defun number->digits (n)
  (defun iter (n res)
    (if (zerop n)
      res
      (iter (truncate (/ n 10)) (cons (rem n 10) res))))
  (iter n '()))


(defun digits->number (s)
  (reduce #'(lambda (res d) (+ d (* res 10))) 
          s
          :initial-value 0))

;; end of file
