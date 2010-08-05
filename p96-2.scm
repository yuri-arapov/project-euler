;; 2010-07-26
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=96
;;
;; Problem 96
;; 27 May 2005
;;
;; Su Doku (Japanese meaning number place) is the name given to a popular
;; puzzle concept. Its origin is unclear, but credit must be attributed to
;; Leonhard Euler who invented a similar, and much more difficult, puzzle idea
;; called Latin Squares. The objective of Su Doku puzzles, however, is to
;; replace the blanks (or zeros) in a 9 by 9 grid in such that each row,
;; column, and 3 by 3 box contains each of the digits 1 to 9. Below is an
;; example of a typical starting puzzle grid and its solution grid.
;;
;;   003 020 600     483 921 657
;;   900 305 001     967 345 821
;;   001 806 400     251 876 493
;;                              
;;   008 102 900     548 132 976
;;   700 000 008     729 564 138
;;   006 708 200     136 798 245
;;                              
;;   002 609 500     372 689 514
;;   800 203 009     814 253 769
;;   005 010 300     695 417 382
;;
;; A well constructed Su Doku puzzle has a unique solution and can be solved by
;; logic, although it may be necessary to employ "guess and test" methods in
;; order to eliminate options (there is much contested opinion over this). The
;; complexity of the search determines the difficulty of the puzzle; the
;; example above is considered easy because it can be solved by straight
;; forward direct deduction.
;;
;; The 6K text file, sudoku.txt (right click and 'Save Link/Tarboard-value As...'),
;; contains fifty different Su Doku puzzles ranging in difficulty, but all with
;; unique solutions (the first puzzle in the file is the example above).
;;
;; By solving all fifty puzzles find the sum of the 3-digit numbers found in
;; the top left corner of each solution grid; for example, 483 is the 3-digit
;; number found in the top left corner of the solution grid above.
;;
;; Answer: 24702
;;


;; Guile specific.
(use-modules (srfi srfi-11)) ;; (let-values ...)


(load "range.scm")
(load "product.scm")
(load "uniq.scm")


;; Cell position: row and column.
;;
(define (make-pos row col)  (cons row col))
(define (row pos)           (car pos))
(define (col pos)           (cdr pos))


;; Position <-> plain index.
;;
(define (pos->index pos)    (+ (* 9 (row pos)) (col pos)))
(define (index->pos index)  (make-pos (quotient index 9) (remainder index 9)))


;; Product of indicies -> list of positions.
;;
(define (product s1 s2)     (each-to-each make-pos s1 s2))


;; Position -> list of position of its row, column, and box.
;;
(define (pos->row pos)      (product (list (row pos)) (range 0 8)))
(define (pos->col pos)      (product (range 0 8) (list (col pos))))

(define (pos->box pos)
  (define (cell-indices box-index) 
    (map (lambda (i) (+ i (* 3 box-index))) '(0 1 2)))
  (product (cell-indices (quotient (row pos) 3)) 
           (cell-indices (quotient (col pos) 3))))


;; All the position given pos is affected.
;;
(define (pos->buddies pos)
  (append
    (pos->row pos)
    (pos->col pos)
    (pos->box pos)))


;; All the positions of sudoku board.
;;
(define *all-positions* (product (range 0 8) (range 0 8)))


;; Board cell: value and list of possible candidates.
;;
(define (make-cell value candidates)    (cons value (list candidates)))
(define (cell-value cell)               (car cell))
(define (cell-candidates cell)          (car (cdr cell)))
(define (cell-blank? cell)              (zero? (cell-value cell)))


;; List of ints -> board.
;;
(define (make-board ls) 
  (let ((board (list->vector 
                 (map (lambda (n) (make-cell n '())) 
                      ls))))

    ;; True if given value v is allowed to be placed on given position.
    (define (allowed? pos v)
      (every
        (lambda (p)
          (not (= v (board-value board p))))
        (pos->buddies pos)))

    (board-map
      (lambda (pos cell)
        (if (not (cell-blank? cell))
          cell
          (make-cell (cell-value cell)
                     (filter
                       (lambda (c)
                         (allowed? pos c))
                       (range 1 9)))))
      board)))


;; Board accessors.
;;
(define (board-cell board pos)          (vector-ref board (pos->index pos)))
(define (board-value board pos)         (cell-value (board-cell board pos)))
(define (board-candidates board pos)    (cell-candidates (board-cell board pos)))


;; Map board: apply given proc to every board cell and return new board.
;; proc is (proc pos cell) function.
;;
(define (board-map proc board)
  (list->vector
    (map (lambda (pos)
           (proc pos (board-cell board pos)))
         *all-positions*)))


;; Store given value val at the position pos, update candidates
;; of all affected board cells accordingly,
;; return new board.
;;
(define (board-store board pos val) 
  (let ((index      (pos->index pos))
        (buddies    (uniq (sort (map pos->index (pos->buddies pos)) <))))
    (board-map
      (lambda (pos cell)
        (let ((i (pos->index pos)))
          (cond 

            ;; the cell with new value
            ((= i index)    
             (make-cell val
                        (delete val (cell-candidates cell))))

            ;; the cell must adjust candidates list
            ((member i buddies) 
             (make-cell (cell-value cell)
                        (delete val (cell-candidates cell))))

            ;; the cell is not affected
            (else   
              cell))))

      board)))


;; Return position of the most complete blank cell (i.e. its list of 
;; candidates is the shortest.
;; Return false if no such cell (all the cells are non-blank).
;;
(define (most-complete-cell board)
  (fold
    (lambda (pos res)
      (let ((cell (board-cell board pos)))
        (cond 
          ((not (cell-blank? cell)) res)
          ((not res) pos)
          (else
            (if (< (length (cell-candidates cell)) 
                   (length (board-candidates board res)))
              pos
              res)))))
    #f
    *all-positions*))


;; Solve sudoku and return filled board.
;; Return #f if failed to solve.
;;
;; Find most complete cell, ant try to store its candidates
;; inplace.  Continue recursively.
;;
;; Return solved board or #f in not solved.
;;
(define (solve-sudoku board)
  (let ((pos (most-complete-cell board)))
    (if (not pos)
      board
      (let loop ((candidates (board-candidates board pos)))
;;        (format #t "\n")
;;        (print-boards board)
;;        (format #t "pos ~a, candidates ~a\n" pos candidates)
;;        (read-char)
        (if (null? candidates)
          #f
          (let ((solved-board (solve-sudoku (board-store board pos (car candidates)))))
            (if solved-board
              solved-board
              (loop (cdr candidates)))))))))


;; Test board.
;;
(define (test-board)
  (make-board '(
0 0 3  0 2 0  6 0 0
9 0 0  3 0 5  0 0 1
0 0 1  8 0 6  4 0 0
                   
0 0 8  1 0 2  9 0 0
7 0 0  0 0 0  0 0 8
0 0 6  7 0 8  2 0 0
                   
0 0 2  6 0 9  5 0 0
8 0 0  2 0 3  0 0 9
0 0 5  0 1 0  3 0 0
)))


;; Another test board.
;;
(define (test-board2)
  (make-board '(
2 0 0  0 8 0  3 0 0 
0 6 0  0 7 0  0 8 4 
0 3 0  5 0 0  2 0 9 
              
0 0 0  1 0 5  4 0 8 
0 0 0  0 0 0  0 0 0 
4 0 2  7 0 6  0 0 0 
              
3 0 1  0 0 7  0 4 0 
7 2 0  0 4 0  0 6 0 
0 0 4  0 1 0  0 0 3 
)))


;; Solve test sudoku game.
;;
(define (test)
  (let* ((board (test-board))
         (res   (solve-sudoku board)))
    (print-boards board res)
    (left-top-number res)))


;; Print list of sudoku boards (one next to another).
;;
(define (print-boards . boards)
  (for-each 
    (lambda (row) ;; print given row of each board
      (fold
        (lambda (board n) ;; n is number of the board in a row starting from 0
          (if (> n 0)
            (format #t "    "))  ;; separator between boards
          (for-each
            (lambda (col) ;; print given (row,col) cell of the board
              (format #t "~a " (board-value board (make-pos row col)))
              (if (zero? (remainder (1+ col) 3))
                (format #t " ")))
            (range 0 8)) ;; for each column
          (1+ n))
        0
        boards) ;; for each board
      (format #t "\n")
      (if (and (< row 8) (zero? (remainder (1+ row) 3)))
        (format #t "\n")))
    (range 0 8))) ;; for each row


;; Return top-left 3-digit number of sudoku board.
;;
(define (left-top-number board)
  (+ (* 100 (board-value board (make-pos 0 0)))
     (* 10  (board-value board (make-pos 0 1)))
     (* 1   (board-value board (make-pos 0 2)))))


;; Solve all the sudoku games from given file.
;; Rerturn sum of top-left numbers of solutions.
;;
;; Testing: 
;;   (solve-sudoku-file "sudoku-1.txt")
;;   (solve-sudoku-file "sudoku-2.txt")
;;
(define (solve-sudoku-file file)

  ;; Read board from the file.
  ;; Return (values name board) or #f.
  (define (read-board port)
    ;; Turn list of lines into board.
    (define (lines->board lines)
      (make-board
        (map (lambda (c)
               (- (char->integer c) 48))
             (string->list (string-concatenate lines)))))
    ;; Turn 10 lines into (values name board).
    (define (make-res lines)
      (values (car lines)
              (lines->board (cdr lines))))
    ;; Reading loop.
    (let loop ((lines '()))
      (if (= 10 (length lines))
        (make-res (reverse lines))
        (let ((l (read-line port)))
          (cond
            ((eof-object? l)
             (if (not (null? lines))
               (format #t "failed to read sudoku file"))
             (values #f #f))
            (else
              (loop (cons l lines))))))))

  ;; Read board from port and solve it; accumulate sum of 
  ;; top-left numbers of solutions.
  (define (read-and-solve port)
    (let loop ((sum 0))
      (let-values (((board-name board) (read-board port)))
        (cond 
          ((not board)
           sum)
          (else
            (format #t "~a\n" board-name)
            (and-let* ((solved-board (solve-sudoku board))
                       (x            (left-top-number solved-board)))
              (print-boards board solved-board)
              (format #t "~a\n\n" x)
              (loop (+ sum x))))))))

  (call-with-input-file file read-and-solve))


;; Solve problem 96.
;;
(define (p96)
  (solve-sudoku-file "sudoku.txt"))

;; end of file
;; vim: ts=4 sw=4
