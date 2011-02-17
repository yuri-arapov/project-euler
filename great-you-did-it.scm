;; GREA + TYOU = DIDIT


(load "permutations.scm")
(load "uniq.scm")


(define (curry proc . args)
  (lambda rest-args (apply proc (append args rest-args))))


(define (digits->number dd)
  (fold (lambda (d res) (+ d (* res 10))) 0 dd))


(define (char->digit alphabet digits ch)
  (if (char=? ch (car alphabet))
    (car digits)
    (char->digit (cdr alphabet) (cdr digits) ch)))


(define (word->number alphabet digits word)
  (digits->number (map (curry char->digit alphabet digits) (string->list word))))


(define (bingo? alphabet digits)
  (and 
    (not (any zero? (map (curry char->digit alphabet digits) '(#\G #\T #\D))))
    (let* ((w->n  (curry word->number alphabet digits))
           (grea  (w->n  "GREA"))
           (tyou  (w->n  "TYOU"))
           (didit (w->n "DIDIT")))
      (and (= (+ grea tyou) didit)
           (list grea tyou didit)))))


(define (solve-ext alphabet digits-ls)
  (let loop ((dd digits-ls))
    (and (not (null? dd))
         (or (bingo? alphabet (car dd))
             (loop (cdr dd))))))


(define alphabet (uniqp (sort (string->list "GREATYOUDIDIT") char<?) char=?))


(define (solve)
  (time (solve-ext alphabet (time (permutations '(1 2 3 4 5 6 7 8 9 0))))))


;; end of file
;; vim: ts=4 sw=4 et
