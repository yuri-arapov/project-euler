;; Feb. 16, 2015
;;
;; Searching a triangular array for a sub-triangle having minimum-sum
;;
;; Problem 150
;;
;; In a triangular array of positive and negative integers, we wish to find a
;; sub-triangle such that the sum of the numbers it contains is the smallest
;; possible.
;;
;; In the example below, it can be easily verified that the marked triangle
;; satisfies this condition having a sum of −42.
;;
;;       15
;;      -14  -7
;;       20 -13  -5
;;       -3   8  23 -26
;;        1  -4  -5 -18   5
;;      -16  31   2   9  28   3
;;
;; We wish to make such a triangular array with one thousand rows, so we generate
;; 500500 pseudo-random numbers sk in the range ±219, using a type of random
;; number generator (known as a Linear Congruential Generator) as follows:
;;
;; t := 0
;; for k = 1 up to k = 500500:
;;     t := (615949*t + 797807) modulo 220
;;     sk := t−219
;;
;; Thus: s1 = 273519, s2 = −153582, s3 = 450905 etc
;;
;; Our triangular array is then formed using the pseudo-random numbers thus:
;;
;; s1
;; s2  s3
;; s4  s5  s6
;; s7  s8  s9  s10
;; ...
;;
;; Sub-triangles can start at any element of the array and extend down as far as
;; we like (taking-in the two elements directly below it from the next row, the
;; three elements directly below from the row after that, and so on).
;; The "sum of a sub-triangle" is defined as the sum of all the elements it
;; contains.  Find the smallest possible sub-triangle sum.
;;
;;
;; Answer: -271248680
;;
;; NOTE: I didn't wait the search to finish, I just tried each min sum as it
;;       appeared during execution.  The current row as about 9.


(define (make-tri size)
  (let ((t (make-vector (* (1+ size) (1+ size)) 0)))
    (vector-set! t 0 size)
    t))

(define (tri-size t) (vector-ref t 0))
(define (tri-idx t r c) (+ c (* r (+ 1 (tri-size t)))))
(define (tri-ref t row col) (vector-ref t (tri-idx t row col)))
(define (tri-set! t row col v) (vector-set! t (tri-idx t row col) v))
(define (tri-copy t) (list->vector (vector->list t)))



(define (lcg limit on-item end init)
  (let ((two^20 (expt 2 20))
        (two^19 (expt 2 19)))
    (let loop ((n 1) (t 0) (res init))
      (if (> n limit) (end res)
        (let* ((tt (remainder (+ (* 615949 t) 797807) two^20))
               (s (- tt two^19)))
          (loop (1+ n) tt (on-item n s res)))))))


(define (generate-triange size)
  (define (size->count size)
    (if (even? size) (* (+ size 1) (/ size 2))
      (+ (* (+ size 1) (quotient size 2)) (/ (+ size 1) 2))))

  (let ((t (make-tri size)))
    (let loop ((row 1)
               (s (lcg (size->count size)
                       (lambda (n s res) (cons s res))
                       reverse
                       '())))
      (if (> row size) t
        (loop (1+ row)
              (let fill ((c 1) (e s))
                (if (> c row) e
                  (begin
                    (tri-set! t row c (car e))
                    (fill (1+ c) (cdr e))))))))))



(define test-triangle
  '#(6   0   0   0   0   0   0
     0  15   0   0   0   0   0
     0 -14  -7   0   0   0   0
     0  20 -13  -5   0   0   0
     0  -3   8  23 -26   0   0
     0   1  -4  -5 -18   5   0
     0 -16  31   2   9  28   3))


(define (prepare-triangle! t)
  (dotimes (row 1 (tri-size t))
    (dotimes (k 1 row)
      (tri-set! t row k (+ (tri-ref t row (- k 1)) (tri-ref t row k)))))
  t)



(define (min-sub-tri t)
  (let* ((tt (prepare-triangle! (tri-copy t)))
         (size (tri-size t))
         (res (tri-ref t 1 1))

         (sub-tri-sum
           (lambda (r c sz)
             (let loop ((i 0) (sum 0))
               (if (= i sz) (begin
                              (format #t "~a ~a ~a ~a   ~a              \r"
                                      r c sz sum res)
                              sum)
                 (loop (1+ i) (+ sum
                                 (- (tri-ref tt (+ r i) (+ c i))
                                    (tri-ref tt (+ r i) (- c 1))))))))))

    (dotimes (row 1 size)
      (dotimes (col 1 row)
        (dotimes (sz 1 (+ 1 (- size row)))
          (set! res (min res (sub-tri-sum row col sz))))))
    (format #t "\n")
    res))


(define (p150)
  (min-sub-tri (generate-triange 1000)))


;; end of file
