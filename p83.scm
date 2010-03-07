;; 2010-03-02
;; 2010-03-07
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=83
;;
;; Problem 83
;; 19 November 2004
;;
;; NOTE: This problem is a significantly more challenging version of Problem 81.
;;
;; In the 5 by 5 matrix below, the minimal path sum from the top left to the
;; bottom right, by moving left, right, up, and down, is indicated in bold red
;; and is equal to 2297.
;; 
;;  *131*   673    *234*   *103*    *18*
;;  *201*   *96*   *342*    965    *150*
;;   630    803     746    *422*   *111*
;;   537    699     497    *121*    956
;;   805    732     524     *37*   *331*
;;
;; Find the minimal path sum, in matrix.txt (right click and 'Save Link/Target
;; As...'), a 31K text file containing a 80 by 80 matrix, from the top left to
;; the bottom right by moving left, right, up, and down.
;;
;; Answer: 425185


;; Cell constructor and accessors.
;;
(define (make-cell r c) (cons r c))
(define (cell-r cell)   (car cell))
(define (cell-c cell)   (cdr cell))


;; Combination (addition) of two cells.
;;
(define (cell-combine c1 c2)
  (make-cell (+ (cell-r c1) (cell-r c2))
             (+ (cell-c c1) (cell-c c2))))


;; Turn direction into cell.
;;
(define (cell-dir->cell dir)
  (cond ((eqv? dir 'east)   (make-cell  0  1))
        ((eqv? dir 'west)   (make-cell  0 -1))
        ((eqv? dir 'south)  (make-cell  1  0))
        ((eqv? dir 'north)  (make-cell -1  0))
        (else               (error "cell-dir->cell: bad arg:" dir))))


;; Move cell into given direction.
;;
(define (cell-move cell dir)
  (cell-combine cell (cell-dir->cell dir)))


;; Matrix constructor.
;;
(define (make-matrix rows columns init-val)
  (list->vector
    (list-tabulate 
      rows
      (lambda (n) (make-vector columns init-val)))))


;; Matrix accessors and modifiers.
;;
(define (matrix-rows    m)              (vector-length m))
(define (matrix-columns m)              (vector-length (vector-ref m 0)))
(define (matrix-ref     m row col)      (vector-ref    (vector-ref m row) col))
(define (matrix-set!    m row col val)  (vector-set!   (vector-ref m row) col val))


;; Matrix accessors/modifiers by the cell.
;;
(define (cell-matrix-ref  m cell)       (matrix-ref  m (cell-r cell) (cell-c cell)))
(define (cell-matrix-set! m cell val)   (matrix-set! m (cell-r cell) (cell-c cell) val))


;; Read rectangular table of comma-separated numbers.
;;
(define (read-matrix fname)
  (call-with-input-file 
    fname
    (lambda (f)
      (let loop ((line (read-line f))
                 (rows '()))
        (if (eof-object? line)
          (list->vector (reverse rows))
          (let* ((a (string-split line #\,))
                 (b (list->vector (map string->number a))))
            (loop (read-line f)
                  (cons b rows))))))))


;; Faster (I guess) way to do
;;   (delete-duplicates (append s1 s2))
;; assuming that there're no duplicates in s1 and s2.
;;
(define (join s1 s2)
  (cond ((null? s2) s1)
        ((null? s1) s2)
        (else
          (join (if (member (car s2) s1)
                  s1
                  (cons (car s2) s1))
                (cdr s2)))))


;; Find the shortest way between north-west and south-east corners of the
;; matrix assuming that we may go north, east, south, or west from any given
;; cell.
;;
(define (find-shortest-path data)

  (define (data-ref cell) (cell-matrix-ref data cell))

  (define gugol (expt 10 10))   ;; very large number

  (let ((max-row (1- (matrix-rows    data)))
        (max-col (1- (matrix-columns data))))

    (define north-west (make-cell 0 0))
    (define south-east (make-cell max-row max-col))

    (define (good-cell? cell) (and (<= 0 (cell-r cell) max-row)
                                   (<= 0 (cell-c cell) max-col)))

    (define  path               (make-matrix (1+ max-row) (1+ max-col) gugol))
    (define (path-ref  cell)    (cell-matrix-ref  path cell))
    (define (path-set! cell val)(cell-matrix-set! path cell val))

    (define  solved             (make-matrix (1+ max-row) (1+ max-col) #f))
    (define (solved? cell)      (cell-matrix-ref solved  cell))
    (define (solved-set! cell)  (cell-matrix-set! solved cell #t))

    (define max-working-cells 0)

    (define (find-best-cell cells)
      (set! max-working-cells (max max-working-cells (length cells)))
      (let loop ((s   (cdr cells))
                 (res (car cells)))
        (if (null? s)
          res
          (loop (cdr s)
                (if (< (path-ref (car s)) (path-ref res))
                  (car s)
                  res)))))

    (define (iter working-cells)
      (if (null? working-cells)
        (begin
          (format #t "debug: max length of cells list: ~a\n" max-working-cells)
          (path-ref south-east))
        (let ((cell (find-best-cell working-cells)))
          (let ((neighbours 
                  (filter (lambda (c) (and (good-cell? c) (not (solved? c))))
                          (map (lambda (dir) (cell-move cell dir))
                               (list 'north 'east 'south 'west)))))

            (for-each (lambda (c)
                        (path-set! c (min (+ (path-ref cell) (data-ref c))
                                          (path-ref c))))
                      neighbours)
            (solved-set! cell)
            (iter (join (delete cell working-cells) neighbours))))))

    ;; seed initial node
    (path-set! north-west (data-ref north-west))

    (iter (list north-west))))
      

(define (p83-file file)
  (let ((data (read-matrix file)))
    (find-shortest-path data)))


(define (p83)
  (p83-file "matrix.txt"))


;; end of file
