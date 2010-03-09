;; 2010-03-02
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=82
;;
;; Problem 82
;; 05 November 2004
;;
;; NOTE: This problem is a more challenging version of Problem 81.
;;
;; The minimal path sum in the 5 by 5 matrix below, by starting in any cell in
;; the left column and finishing in any cell in the right column, and only
;; moving up, down, and right, is indicated in red and bold; the sum is equal
;; to 994.
;;
;;
;;    131    673   *234*  *103*   *18*
;;   *201*   *96*  *342*   965    150
;;    630    803    746    422    111
;;    537    699    497    121    956
;;    805    732    524     37    331
;;
;; Find the minimal path sum, in matrix.txt (right click and 'Save Link/Target
;; As...'), a 31K text file containing a 80 by 80 matrix, from the left column
;; to the right column.
;;
;; Answer: ???


(use-modules (ice-9 receive))


(define (make-matrix rows columns init-val)
  (list->vector
    (list-tabulate rows
                   (lambda (n) (make-vector columns init-val)))))


(define (matrix-rows    m)              (vector-length m))
(define (matrix-columns m)              (vector-length (vector-ref m 0)))
(define (matrix-ref     m row col)      (vector-ref    (vector-ref m row) col))
(define (matrix-set!    m row col val)  (vector-set!   (vector-ref m row) col val))


(define (read-matrix fname)
  (call-with-input-file fname
                        (lambda (f)
                          (let loop ((line (read-line f))
                                     (matrix '()))
                            (if (eof-object? line)
                              (list->vector (reverse matrix))
                              (let* ((a (string-split line #\,))
                                     (b (list->vector (map string->number a))))
                                (loop (read-line f)
                                      (cons b matrix))))))))


(define (make-cell r c) (cons r c))
(define (cell?  cell)   (pair? cell))
(define (cell-r cell)   (car cell))
(define (cell-c cell)   (cdr cell))

(define (cell-combine c1 c2)
  (make-cell (+ (cell-r c1) (cell-r c2))
             (+ (cell-c c1) (cell-c c2))))

(define (cell-dir->cell dir)
  (cond ((eqv? dir 'right) (make-cell  0 1))
        ((eqv? dir 'down)  (make-cell  1 0))
        ((eqv? dir 'up)    (make-cell -1 0))
        (else              (error "cell-dir->cell: bad arg:" dir))))


(define (cell-move cell dir)
  (cell-combine cell (cell-dir->cell dir)))


(define (find-shortest-path m starting-row)
;; stack-on-heap version of recursive function
;;

  (define (data-ref cell) (matrix-ref m (cell-r cell) (cell-c cell)))

  (define top-left (make-cell 0 0))

  (define (push cell stack) (cons cell stack))
  (define (pop stack) (cdr stack))
  (define (top stack) (car stack))

  (define (in-stack? cell stack) (member cell stack))

  (let ((max-row (1- (matrix-rows m)))
        (max-col (1- (matrix-columns m))))

    (define (in? cell) (and (<= 0 (cell-r cell) max-row)
                            (<= 0 (cell-c cell) max-col)))

    (define (bottom-right? cell) (and (= (cell-r cell) max-row)
                                      (= (cell-c cell) max-col)))

    (define (right-border? cell) (= (cell-c cell) max-col))

    (define  memo               (make-matrix (1+ max-row) (1+ max-col) #f))
    (define (memo-ref  cell)    (matrix-ref  memo (cell-r cell) (cell-c cell)))
    (define (memo-set! cell val)(matrix-set! memo (cell-r cell) (cell-c cell) val))

    (define (res)
      (let ((x (list-tabulate 
                 (1+ max-row)
                 (lambda (r) (matrix-ref memo r 0)))))
        (format #t "~a\n" x)
        (apply min x)))

    (define (shift-format stack)
      (for-each (lambda (x) (format #t " "))
                (iota (1- (length stack)))))

    (define (iter stack)
      (if (null? stack)                                                         ; stack empty -- all done
        (res)
        (let ((cell (top stack)))                                               ; topmost stack element
          (shift-format stack)
          (format #t "cell: ~a ~a\n" (cell-r cell) (cell-c cell))
          (cond ((memo-ref cell)
                 (iter (pop stack)))                                            ; cell aready processed

                ((right-border? cell)                                           ; cell on right border
                 (memo-set! cell (data-ref cell))                               ; no need to move further
                 (shift-format stack)
                 (format #t "cell: ~a ~a x\n" (cell-r cell) (cell-c cell))
                 (iter (pop stack)))

                (else
                  (let ((all (filter (lambda (c)                                ; all the cells we can go
                                       (and (in? c)                             ; to from given cell
                                            (not (in-stack? c stack))))         ; (outer cells and cells
                                                                                ; under processing are
                                                                                ; filtered out)
                                     ; possible cells to move to
                                     (map (lambda (dir) (cell-move cell dir))

                                          ; possible move directions
                                          (list 'up 'right 'down)))))
                                          ;;(list 'down 'right 'up)))))

                    (receive (known unknown)                                    ; separate known cells from
                             (partition memo-ref all)                           ; unknown ones

                      (cond ((null? unknown)                                    ; all the cell known:
                             (memo-set! cell                                    ; compute and memorize
                                        (+ (data-ref cell)                      ; value of given cell;
                                           (apply min (map memo-ref known))))   ;
                             (shift-format stack)
                             (format #t "cell: ~a ~a x\n" (cell-r cell) (cell-c cell))
                             (iter (pop stack)))                                ; move on

                            (else                                               ; there are unknown cell:
                              (iter (fold (lambda (c st) (push c st))           ; push them to the stack
                                          stack                                 ; and continue
                                          unknown)))))))))))                    ; 

    (format #t "row ~a\n" starting-row)
    (iter (push (make-cell starting-row 0) '()))))


(define (p82)
  ;;;(let ((m (read-matrix "matrix.txt")))
  (let ((m (read-matrix "matrix-small.txt")))
    (apply min
           (map (lambda (r) (find-shortest-path m r))
                (iota (matrix-rows m))))))


;; end of file
