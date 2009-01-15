;; the-same.scm
;;
;; (the-same? a-list) function returns #t if either
;; all the elements of a-list list are the same, or
;; list is empty.
;;
;; example:
;;
;;   (the-same? '())      -> #t
;;   (the-aame? '(1 1 1)) -> #t
;;   (the-same? '(1 2 3)) -> #f
;;   (the-same? 123)      -> #f  (not a list)


(define (the-same1? ls)
;; way too slow comparing to the-same (iterative version, see below)
;;
  (cond ((not (list? ls)) #f)
        ((null? ls) #t)
        ((null? (cdr ls)) #t)
        ((equal? (car ls) (cadr ls)) (the-same1? (cdr ls)))
        (else #f)))


(define (the-same? ls)
;; iterative version
;;
  (define (iter first ls)
    (cond ((null? ls) #t)
          ((equal? first (car ls)) (iter first (cdr ls)))
          (else #f)))
  (cond ((not (list? ls)) #f)
        ((null? ls) #t)
        (else (iter (car ls) (cdr ls)))))


(define (time-test proc)
  (let* ((start (current-time))
         (dummy (proc))
         (stop  (current-time)))
    (- stop start)))


;; end of file
