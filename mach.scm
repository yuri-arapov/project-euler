;; Machinarium puzzle solver
;;


(use-modules (srfi srfi-2)) ;; (and-let* ...)


;; 0 - black
;; 1 - red
;; 2 - green
;; 3 - yellow
(define puzzle1
  '#(0 0 0 0 0
     0 0 0 0 0
     0 0 0 0 0
     0 0 0 0 0
     0 0 0 0 0))
(define puzzle2
  '#(0 0 0 0 0
     0 0 0 1 0
     0 0 0 0 0
     0 0 0 0 0
     0 0 0 0 1))
(define puzzle3
  '#(0 0 0 0 0
     0 0 0 0 0
     0 0 1 0 1
     0 0 0 0 0
     0 0 0 0 0))
(define puzzle4
  '#(0 0 0 0 0
     0 1 0 1 0
     0 0 0 0 0
     0 1 1 0 1
     0 0 0 0 1))
(define puzzle5
  '#(0 0 0 0 1
     0 0 0 0 0
     0 0 0 1 0
     1 0 0 0 0
     0 0 1 0 0))
(define puzzle6 
  '#(0 0 1 1 1
     0 0 0 0 0
     0 0 0 0 0
     0 0 0 0 0
     1 1 0 0 0))


(define black  0)
(define red    1)
(define green  2)
(define yellow 3)


(define (make-pos row col) (cons row col))
(define (pos-row pos) (car pos))
(define (pos-col pos) (cdr pos))


(define (pos=? p1 p2) (= (pos->index p1) (pos->index p2)))


(define (pos-good? pos) (and (<= 0 (pos-row pos) 4)
                             (<= 0 (pos-col pos) 4)))


(define (pos->index pos) (+ (* (pos-row pos) 5) (pos-col pos)))


(define (index->pos index) (make-pos (quotient index 5) (remainder index 5)))


;; dir is one of 'left 'right 'up 'down
(define (pos-move pos dir)
  (let loop ((s (list (values 'left   0 -1) 
                      (values 'right  0  1)
                      (values 'up    -1  0)
                      (values 'down   1  0))))
    (if (null? s)
      (error "pos-move: bad dir" dir)
      (let-values (((d r c) (car s)))
        (if (eq? dir d)
          (make-pos (+ (pos-row pos) r)
                    (+ (pos-col pos) c))
          (loop (cdr s)))))))


(define (puzzle-ref puzzle pos)
  (and (pos-good? pos)
       (vector-ref puzzle (pos->index pos))))


(define (puzzle-set puzzle pos color)
  (let ((new-puzzle (vector-copy puzzle)))
    (vector-set! new-puzzle (pos->index pos) color)
    new-puzzle))


(define (first-pos) (make-pos 0 0))


(define (next-pos pos) (index->pos (1+ (pos->index pos))))


(define (black? puzzle pos) 
  (and (pos-good? pos) (= black (puzzle-ref puzzle pos))))


(define (solved? puzzle)
  (every (lambda (x) (not (= black x)))
         (vector->list puzzle)))


(define (move puzzle pos dir) ;; -> puzzle pos
  (let loop ((puzzle puzzle)
             (pos    pos))
    (let ((new-pos (pos-move pos dir)))
      (if (not (black? puzzle new-pos))
        (values puzzle pos)
        (loop (puzzle-set puzzle new-pos yellow) new-pos)))))

 
(define (find-path puzzle start-pos)

  (define (move-to puzzle pos dir path)
    (let-values (((new-puzzle new-pos) (move puzzle pos dir)))
      (if (pos=? pos new-pos)
        #f
        (move-from new-puzzle new-pos (cons dir path)))))

  (define (move-from puzzle pos path)
    (if (solved? puzzle)
      (reverse path)
      (let loop ((dirs '(left up right down)))
        (if (null? dirs)
          #f
          (or (move-to puzzle pos (car dirs) path)
              (loop (cdr dirs)))))))

  (move-from (puzzle-set puzzle start-pos green) start-pos '()))
             

(define (solve puzzle)
  (let loop ((start-pos (first-pos)))
    (if (not (pos-good? start-pos))
      #f
      (if (not (black? puzzle start-pos))
        (loop (next-pos start-pos))
        (or
          (and-let* ((path (find-path puzzle start-pos)))
            (values start-pos path))
          (loop (next-pos start-pos)))))))


(define (solve-all)
  (map solve (list puzzle1 puzzle2 puzzle3 puzzle4 puzzle5 puzzle6)))

;; end of file
;; vim: ts=4 sw=4 et
