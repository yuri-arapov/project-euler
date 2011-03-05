;; 2011-03-03
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=129
;;
;; Problem 129
;; 27 October 2006
;;
;; A number consisting entirely of ones is called a repunit. We shall defun
;; R(k) to be a repunit of length k; for example, R(6) = 111111.
;;
;; Given that n is a positive integer and GCD(n, 10) = 1, it can be shown that
;; there always exists a value, k, for which R(k) is divisible by n, and let
;; A(n) be the least such value of k; for example, A(7) = 6 and A(41) = 5.
;;
;; The least value of n for which A(n) first exceeds ten is 17.
;;
;; Find the least value of n for which A(n) first exceeds one-million.
;;
;; Answer: 1000023


;; Find multiplier m so that last digit of n*m+x is 1.
;;(defun find-multiplier (n x)
;;  (find-if '#(lambda (m) (= 1 (rem (+ x (* n m)) 10))) '(0 1 2 3 4 5 6 7 8 9)))


;; Map of mutipliers.  See find-multiplier.
;; Obtained as:
;;   (map 
;;     (lambda (n) 
;;       (map (curry find-multiplier n) 
;;            '(0 1 2 3 4 5 6 7 8 9))) 
;;     '(0 1 2 3 4 5 6 7 8 9))
;;
(defvar *mul*
  ; n  0   1   2   3   4   5   6   7   8   9
                                                ; x
'#(#(nil   0 nil nil nil nil nil nil nil nil)   ; 0
   #(  1   0   9   8   7   6   5   4   3   2)   ; 1
   #(nil   0 nil   4 nil   3 nil   2 nil   1)   ; 2
   #(  7   0   3   6   9   2   5   8   1   4)   ; 3
   #(nil   0 nil   2 nil   4 nil   1 nil   3)   ; 4
   #(nil   0 nil nil nil nil   1 nil nil nil)   ; 5
   #(nil   0 nil   3 nil   1 nil   4 nil   2)   ; 6
   #(  3   0   7   4   1   8   5   2   9   6)   ; 7
   #(nil   0 nil   1 nil   2 nil   3 nil   4)   ; 8
   #(  9   0   1   2   3   4   5   6   7   8))) ; 9

(defun mul (n x)
  (aref (aref *mul* (rem n 10)) 
        (rem x 10)))


(defun quotient (n d) (truncate (/ n d)))


;; Test number for being repunit.
(defun repunitp (n)
  (labels ((iter (n)
             (cond ((= n 1)                 t)
                   ((not (= 1 (rem n 10)))  nil)
                   (t                       (iter (quotient n 10))))))
    (iter n)))


;; Determine number of digits of n.
(defun number-of-digits (n)
  (if (zerop n)
    0
    (1+ (number-of-digits (quotient n 10)))))


;; Function A(n) -> k.
(defun A (n)
  (labels ((iter (x count)
             (if (not (mul n x))
               nil
               (let ((y (+ x (* n (mul n x)))))
                 (if (repunitp y)
                   (+ count (number-of-digits y))
                   (iter (quotient y 10) (1+ count)))))))
    (iter 0 0)))


;; Solve problem 129.
;; Since A(n) < n (non-scientific guess) we can start with
;; odd n that greater than 1000001.
(defun p129 ()
  (loop for n = 1000003 then (+ 2 n) 
        do (let ((k (A n)))
             (if (and k (> k 1000000))
               (return n)))))


;; end of file
;; vim: sw=4 ts=4
