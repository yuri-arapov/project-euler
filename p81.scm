;; 09 June 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=81
;;
;; Problem 81
;; 22 October 2004
;;
;; In the 5 by 5 matrix below, the minimal path sum from the top left to the
;; bottom right, by only moving to the right and down, is indicated in red and
;; is equal to 2427.
;;
;;      
;;     *131*    673     234     103      18
;;     *201*    *96*   *342*    965     150
;;      630     803    *746*   *422*    111
;;      537     699     497    *121*    956
;;      805     732     524     *37*   *331*
;;              
;;
;; Find the minimal path sum, in matrix.txt (right click and 'Save Link/Target
;; As...'), a 31K text file containing a 80 by 80 matrix, from the top left to
;; the bottom right by only moving right and down.
;;
;; Answer: 427337
;;
;; TODO: make generic memoization of function calls (see SICP).



(define (make-matrix rows columns init-val)
  (list-tabulate (+ rows 1)
                 (lambda (r)
                   (list-tabulate (+ columns 1) (lambda (c) init-val)))))



(define (matrix-rows m)
  (length m))



(define (matrix-columns m)
  (length (list-ref m 0)))



(define (matrix-ref m row col)
  (list-ref (list-ref m row) col))



(define (matrix-set! m row col val)
  (list-set! (list-ref m row) col val))



(define (read-matrix fname)
  (call-with-input-file fname
                        (lambda (f)
                          (let loop ((line (read-line f))
                                     (matrix '()))
                            (if (eof-object? line)
                              (reverse matrix)
                              (let* ((a (string-split line #\,))
                                     (b (map string->number a)))
                                (loop (read-line f)
                                      (cons b matrix))))))))



(define (find-shortest-path m max-row max-col)
;; recursive version
;;
;; nice and compact, but causes Guile's stack overflow
;;
  (define gugol (expt 10 10))
  (define memo  (make-matrix (+ max-row 2) (+ max-col 2) #f))
  (define (iter r c depth)
    (if (matrix-ref memo r c)
      (matrix-ref memo r c)
      (begin
        (format #t "~a (~a ~a)~%" depth r c)
        (let ((x (cond ((and (= r max-row) (= c max-col))
                        (matrix-ref m r c))

                       ((or (> r max-row) (> c max-col))
                        gugol)

                       (else
                         (let ((this (matrix-ref m r c))
                               (r+1  (iter (+ r 1)    c    (+ depth 1)))
                               (c+1  (iter    r    (+ c 1) (+ depth 1))))
                           (min (+ this r+1) (+ this c+1)))))))
          (matrix-set! memo r c x)
          x))))
  (iter 0 0 0))



(define (find-shortest-path2 m max-row max-col)
;; stack-on-heap version of the function above
;;
  (define (data r c) (matrix-ref m r c))

  (define gugol (expt 10 10))

  (define memo  (make-matrix (+ max-row 2) (+ max-col 2) #f))

  (define (memoized r c) (matrix-ref memo r c))

  (define (memoize r c val) (matrix-set! memo r c val))

  (define (iter stack)
    (if (null? stack)
      (matrix-ref memo 0 0)
      (let* ((top (car stack))
             (r   (first top))
             (c   (second top)))
        (if (memoized r c)
          (iter (cdr stack))
          (cond ((and (= r max-row) (= c max-col))
                 (memoize r c (data r c))
                 (iter (cdr stack)))

                ((or (> r max-row) (> c max-col))
                 (memoize r c gugol)
                 (iter (cdr stack)))

                ((and (memoized r (+ c 1)) (memoized (+ r 1) c))
                 (memoize r c (+ (data r c) (min (memoized r (+ c 1))
                                                 (memoized (+ r 1) c))))
                 (iter (cdr stack)))

                ((and (memoized r (+ c 1)) (not (memoized (+ r 1) c)))
                 (iter (cons (list (+ r 1) c) stack)))

                ((and (not (memoized r (+ c 1))) (memoized (+ r 1) c))
                 (iter (cons (list r (+ c 1)) stack)))

                (else
                  (iter (cons (list r (+ c 1)) (cons (list (+ r 1) c) stack)))))))))

  (iter (cons (list 0 0) '())))



(define (p81)
  (let ((m (read-matrix "matrix.txt")))
    (find-shortest-path2 m 
                         (- (matrix-rows m) 1)
                         (- (matrix-columns m) 1))))


;; end of file
