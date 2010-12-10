;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=10
;;
;; Problem 10
;; 08 February 2002
;;
;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
;;
;; Find the sum of all the primes below two million.
;; Answer: 142913828922
;;
;; FIXME: worth to try: http://en.wikipedia.org/wiki/Sieve_of_Atkin


(defun dividerp (n d) (zerop (rem n d)))

(defun quotient (n d) (truncate (/ n d)))


(defun p10-int (maxn)
  (let ((bv (make-array (1+ maxn) :element-type 'bit :initial-element 1)))

    (defun primep (pos) (not (zerop (aref bv pos))))

    (defun mark (pos) (setf (aref bv pos) 0))

    (defun mark-all (pos)
      (loop for x from 2 to (quotient maxn pos) do
            (mark (* pos x))))

    (format t "ceiving...~%")
    (mark-all 2)
    (do ((n 3 (+ 2 n))) ((>= n maxn))
      (if (primep n)
        (mark-all n)))

    (format t "collecting...~%")
    (do ((n 3 (+ 2 n))
         (res 2 (if (primep n) (+ res n) res)))
      ((>= n maxn) res))))


(defun p10 () (p10-int 2000000))


;; end of file
;; vim: ts=4 sw=4 et
