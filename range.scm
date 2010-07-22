;; usefull stuff
;;
;; (range from to) function
;; IMPORTANT: 'to' is included


(define (range from to)
  (let loop ((n to)
             (ls '()))
    (if (> from n)
      ls
      (loop (- n 1) (cons n ls)))))



;;;;(define (range from to) (iota (+ 1 (- to from)) from))
;; get some SRFI-1 sugar
;;
;; NOTE: too slow
;;

;; end of file
