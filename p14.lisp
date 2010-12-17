;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=14
;;
;; Problem 14
;; 05 April 2002
;;
;; The following iterative sequence is defund for the set of positive integers:
;;
;;   n -> n/2    (n is even)
;;   n -> 3n + 1 (n is odd)
;;
;; Using the rule above and starting with 13, we generate the following sequence:
;; 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
;;
;; It can be seen that this sequence (starting at 13 and finishing at 1)
;; contains 10 terms. Although it has not been proved yet (Collatz Problem), it
;; is thought that all starting numbers finish at 1.
;;
;; Which starting number, under one million, produces the longest chain?
;;
;; NOTE: Once the chain starts the terms are allowed to go above one million.
;;
;; Answer: 837799



(defun p14 ()
  (let ((ht (make-hash-table)))

    (defun len-exists? (n)
      (gethash n ht))

    (defun len-ref (n) 
      (gethash n ht))

    (defun len-set! (n len)
      (setf (gethash n ht) len))

    (defun next (n) (if (oddp n) 
                      (+ 1 (* n 3)) 
                      (/ n 2)))

    (defun chain-len (seed)
      (loop for n = seed then (next n) 
            for l = 0 then (1+ l) do
            (if (len-exists? n)
              (let ((ll (+ l (len-ref n))))
                (len-set! seed ll)
                (return-from chain-len ll)))))

    (len-set! 1 1)

    (loop for n from 1 to 999999 
          for len = (chain-len n) then (chain-len n) 
          for res = 0 then (if (> len max-len) n res) 
          for max-len = 0 then (max max-len len)
          finally (return (values res max-len)))))


;; end of file
;; vim: ts=4 sw=4 et
