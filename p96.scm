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
;; The 6K text file, sudoku.txt (right click and 'Save Link/Target As...'),
;; contains fifty different Su Doku puzzles ranging in difficulty, but all with
;; unique solutions (the first puzzle in the file is the example above).
;;
;; By solving all fifty puzzles find the sum of the 3-digit numbers found in
;; the top left corner of each solution grid; for example, 483 is the 3-digit
;; number found in the top left corner of the solution grid above.
;;
;; Answer: 24702
;;
;; FIXME: bruteforce
;;                      


;; Guile specific.
(use-modules (srfi srfi-11)) ;; (let-values ...)


(load "range.scm")
(load "product.scm")


;; Shuffle list elements
;;
(define (shuffle ls)
  (define (set-two v i vi j vj)
    (vector-set! v i vi)
    (vector-set! v j vj)
    v)
  (define (swap v i j)
    (set-two v
             j (vector-ref v i)
             i (vector-ref v j)))
  (define (shuffle-vector v)
    (let ((n (vector-length v)))
      (let loop ((i 0) (v v))
        (if (= i n)
          v
          (loop (1+ i) 
                (swap v (random n) (random n)))))))
  (vector->list (shuffle-vector (list->vector ls))))


(define (index row col) (+ (* 9 (1- row)) (1- col)))
(define (get board row col) (vector-ref board (index row col)))
(define (set board row col n) (vector-set! board (index row col) n) board)
(define (store board row col n) (set (vector-copy board) row col n))
(define (blank? board row col) (zero? (get board row col)))

(define (make-board ls) (list->vector ls))


;; Return true if given number n is allowed to be placed on
;; row,col cell of sudoku board.
;;
(define (allowed? board row col n)

  (define (first-box-index i) (1+ (* 3 (quotient (1- i) 3))))

  (define (allowed? r-ls c-ls)
    (every
      (lambda (i)
        (not (= n (get board (car i) (cadr i)))))
      (product r-ls c-ls)))

  (let ((f-row (first-box-index row))
        (f-col (first-box-index col)))
    (and (allowed? (list row) (range 1 9))
         (allowed? (range 1 9) (list col))
         (allowed? (range f-row (+ 2 f-row)) 
                   (range f-col (+ 2 f-col))))))


;; Solve sudoku and return filled board.
;; Return #f if failed to solve.
;;
(define (solve-sudoku board)

  (define (try-cell board row col)
    (cond 
      ((> row 9)
       board)
      ((> col 9)
       (try-cell board (1+ row) 1))
      ((not (blank? board row col))
       (try-cell board row (1+ col)))
      (else
        (let loop ((n (shuffle (range 1 9))))
          (cond
            ((null? n)
             #f)
            ((not (allowed? board row col (car n)))
             (loop (cdr n)))
            (else
              (let ((new-board (try-cell (store board row col (car n)) row (1+ col))))
                (if new-board
                  new-board
                  (loop (cdr n))))))))))

  (try-cell board 1 1))


;; Solve test sudoku game.
;;
(define (test)
  (let ((res (solve-sudoku (make-board '(
0 0 3  0 2 0  6 0 0
9 0 0  3 0 5  0 0 1
0 0 1  8 0 6  4 0 0
                   
0 0 8  1 0 2  9 0 0
7 0 0  0 0 0  0 0 8
0 0 6  7 0 8  2 0 0
                   
0 0 2  6 0 9  5 0 0
8 0 0  2 0 3  0 0 9
0 0 5  0 1 0  3 0 0
)))))
  (print-boards res)
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
              (format #t "~a " (get board row col))
              (if (zero? (remainder col 3))
                (format #t " ")))
            (range 1 9)) ;; for each column
          (1+ n))
        0
        boards) ;; for each board
      (format #t "\n")
      (if (and (< row 9) (zero? (remainder row 3)))
        (format #t "\n")))
    (range 1 9))) ;; for each row


;; Return top-left 3-digit number of sudoku board.
;;
(define (left-top-number board)
  (+ (* 100 (get board 1 1))
     (* 10  (get board 1 2))
     (* 1   (get board 1 3))))


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
            (or
              (and-let* ((solved-board (solve-sudoku board))
                         (x            (left-top-number solved-board)))
                (print-boards board solved-board)
                (format #t "~a\n\n" x)
                (loop (+ sum x)))
              #f))))))

  (call-with-input-file file read-and-solve))


;; Solve problem 96.
;;
(define (p96)
  (solve-sudoku-file "sudoku.txt"))

;; end of file
