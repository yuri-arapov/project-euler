;; 2010-12-27
;;
;; Project Euler
;;
;; Problem 116
;; 03 March 2006
;;
;; A row of five black square tiles is to have a number of its tiles replaced
;; with coloured oblong tiles chosen from red (length two), green (length
;; three), or blue (length four).
;;
;; If red tiles are chosen there are exactly seven ways this can be done.
;;
;;   xxooo oxxoo ooxxo oooxx
;;   xxxxo xxoxx oxxxx
;;
;; If green tiles are chosen there are three ways.
;;
;;   xxxoo oxxxo ooxxx
;;
;; And if blue tiles are chosen there are two ways.
;;
;;   xxxxo oxxxx
;;
;; Assuming that colours cannot be mixed there are 7 + 3 + 2 = 12 ways of
;; replacing the black tiles in a row measuring five units in length.
;;
;; How many different ways can the black tiles in a row measuring fifty units
;; in length be replaced if colours cannot be mixed and at least one coloured
;; tile must be used?
;;
;; NOTE: This is related to problem 117.
;;
;; Answer: 20492570929


(load "memo.scm")


(define (fill-count color-len black-len)

  (define helper (make-memoized-proc =
    (lambda (len)
      (let loop ((left 0)
                 (acc  0))
        (if (> (+ left color-len) len)
          acc
          (let ((x (helper (- len (+ left color-len)))))
            (loop (1+ left) (+ acc (if (zero? x) 1 (1+ x))))))))))

    (helper black-len))


(define (p116)
  (apply + (map (lambda (c) (fill-count c 50)) '(2 3 4))))


;; end of file
;; vim: ts=4 sw=4 et
