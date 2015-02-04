;; 2010-05-09
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=90
;;
;; Problem 90
;; 04 March 2005
;;
;; Each of the six faces on a cube has a different digit (0 to 9) written on
;; it; the same is done to a second cube. By placing the two cubes side-by-side
;; in different positions we can form a variety of 2-digit numbers.
;;
;; For example, the square number 64 could be formed:
;;
;;  +---+  +---+
;;  | 6 |  | 4 |
;;  +---+  +---+
;;
;; In fact, by carefully choosing the digits on both cubes it is possible to
;; display all of the square numbers below one-hundred: 01, 04, 09, 16, 25, 36,
;; 49, 64, and 81.
;;
;; For example, one way this can be achieved is by placing {0, 5, 6, 7, 8, 9}
;; on one cube and {1, 2, 3, 4, 8, 9} on the other cube.
;;
;; However, for this problem we shall allow the 6 or 9 to be turned upside-down
;; so that an arrangement like {0, 5, 6, 7, 8, 9} and {1, 2, 3, 4, 6, 7} allows
;; for all nine square numbers to be displayed; otherwise it would be
;; impossible to obtain 09.
;;
;; In determining a distinct arrangement we are interested in the digits on
;; each cube, not the order.
;;
;; {1, 2, 3, 4, 5, 6} is equivalent to {3, 6, 4, 1, 2, 5}
;; {1, 2, 3, 4, 5, 6} is distinct from {1, 2, 3, 4, 5, 9}
;;
;; But because we are allowing 6 and 9 to be reversed, the two distinct sets in
;; the last example both represent the extended set {1, 2, 3, 4, 5, 6, 9} for
;; the purpose of forming 2-digit numbers.
;;
;; How many distinct arrangements of the two cubes allow for all of the square
;; numbers to be displayed?
;;
;; Answer: 1217


(load "combinations.scm")


(define (dice->number dice)
  (fold (lambda (n res) 
          (logior res (ash 1 n))) 
        0 
        dice))


(define (square-number? dice1 dice2 square-digits)
  (let ((sd1 (car square-digits))
        (sd2 (car (cdr square-digits))))
    (or (and (logbit? sd1 dice1) (logbit? sd2 dice2))
        (and (logbit? sd2 dice1) (logbit? sd1 dice2)))))


(define (bingo? x y)
  (every 
    (lambda (i)
      (any (lambda (j) (square-number? x y j)) i))
  '(((2 5))
    ((1 8))
    ((3 6) (3 9))
    ((0 1))
    ((0 4))
    ((0 9) (0 6))
    ((1 6) (1 9))
    ((4 9) (4 6))
    ((6 4) (9 4)))))


(define (p90-int)

  (define (loop2 i s)
    (let loop ((s s)
               (res 0))
      (if (null? s) res
        (loop (cdr s)
              (if (bingo? i (car s)) (1+ res)
                res)))))

  (let loop ((s (map dice->number (combinations '(0 1 2 3 4 5 6 7 8 9) 6)))
             (res 0))
    (if (null? s) res
      (loop (cdr s) (+ res (loop2 (car s) (cdr s)))))))


(define (p90)
  (p90-int))


;; end of file
;; vim: sw=4 ts=4
