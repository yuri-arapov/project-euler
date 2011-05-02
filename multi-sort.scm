
(define (multi-sort1 s . less)
  (sort
    s
    (lambda (x y)
      (let loop ((x x) (y y) (less less))
        (if (or (null? x) (null? y) (null? less)) #f
          (if ((car less) (car x) (car y)) #t
            (if (not (equal? (car x) (car y))) #f
              (loop (cdr x) (cdr y) (cdr less)))))))))


(define (multi-sort2 s . less)
  (sort
    s
    (lambda (x y)
      (call/cc
        (lambda (return)
          (fold (lambda (x y less res)
                  (if (less x y) (return #t)
                    (if (not (equal? x y)) (return #f)
                      res)))
                #f
                x y less))))))


(define multi-sort multi-sort2)


(define (ms-test)
  (multi-sort
    '((1 2 3)
      (0 9 1)
      (4 5 6 first)
      (0 0 0)
      (1 4 2)
      (4 9 6)
      (1 4 8)
      (7 8 9)
      (7 9 8)
      (4 5 6 second)
      (1 4 2))
    < > <))


(define (print s) (for-each (curry format #t "~a~%") s))
