;; 31 Aug. 2009
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=85
;;
;; Problem 85
;; 17 December 2004
;; 
;; By counting carefully it can be seen that a rectangular grid measuring 3 by
;; 2 contains eighteen rectangles:
;; 
;; Although there exists no rectangular grid that contains exactly two million
;; rectangles, find the area of the grid with the nearest solution.
;; 
;; Answer: 2772 
;;
;; FIXME: bruteforce


(define (p W H)
  (let loop ((x 1)
             (y 1)
             (s 0))
    (cond ((> x W)
           s)
          ((> y H)
           (loop (+ 1 x) 1 s))
          (else
            (loop x (+ y 1) (+ s (* (- W x -1) (- H y -1))))))))


(define (x)
  (define (loop w h ww hh nn)
;;    (cond ((> w 2000)
    (cond ((> w 100)
           (* ww hh))                   ;; the result
          ((> h w)
           (loop (+ 1 w) 1 ww hh nn))   ;; next loop
          (else
            (let ((n (p w h)))
              (cond ((< (abs (- 2000000 n)) (abs (- 2000000 nn))) ;; n is close to 2000000
                     (format #t "~a ~a ~a (~a)\n" w h n (* w h))
                     (loop w (+ 1 h) w h n))
                    ((> n 2000100)                                ;; too far from 2000000
                     (if (zero? (remainder w 100))                ;; heartbit
                       (format #t "~a - ~a ~a ~a (~a)\n" w ww hh nn (* ww hh)))
                     (loop (+ 1 w) 1 ww hh nn))                   ;; next loop
                    (else
                     (loop w (+ 1 h) ww hh nn)))))))
  (loop 1 1 0 0 0))


;; end of file
