;; 2010-12-28
;;
;; Project Euler
;;
;; Problem 117
;; 10 March 2006
;;
;; Using a combination of black square tiles and oblong tiles chosen from: red
;; tiles measuring two units, green tiles measuring three units, and blue tiles
;; measuring four units, it is possible to tile a row measuring five units in
;; length in exactly fifteen different ways.
;;
;; ooooo rrooo orroo oorro
;; ooorr rrrro rrorr orrrr
;; gggoo ogggo ooggg rrggg
;; gggrr bbbbo obbbb
;;
;; How many ways can a row measuring fifty units in length be tiled?
;;
;; NOTE: This is related to problem 116.
;;
;; Answer: 100808458960497


(load "memo.scm")


(define (fill-count colors black-len)

  (define count-all-colors (make-memoized-proc =
    (lambda (black-len)
      (1+ (apply + (map (lambda (c) (count-one-color c black-len)) 
                        colors))))))

  (define (count-one-color color-len black-len)
    (let loop ((black-len black-len)
               (acc 0))
      (if (> color-len black-len)
        acc
        (loop (1- black-len) (+ acc (count-all-colors (- black-len color-len)))))))

  (count-all-colors black-len))


(define (p117)
  (fill-count '(2 3 4) 50))

;; end of file
;; vim: sw=4 ts=4 et
