;; combinations.scm
;;
;; Unique combinations of list elements.
;;


;; Return number of unique combination of m elements by n.
;;
(define (combinations-count m n)
  (if (= 1 n)
    m
    (* (/ m n) (combinations-count (- m 1) (- n 1)))))


;; Return list of unique combinations of ls list elements by n.
;; Order of elements preserverd.
;; Exmaple:
;;   (combinations '(a b c) 2) ->
;;   ((a b) (a c) (b c))
;;
(define (combinations ls n)
  (define (iter ls n)
    (cond ((zero? n)
           '())
          ((< (length ls) n)
           '())
          ((= n 1)
           (map (lambda (x) (list x)) ls))
          ((= (length ls) n)
           (list ls))
          (else
            (let* ((r1 (iter (cdr ls) (- n 1)))
                   (r2 (iter (cdr ls) n)))
              (append
                (map (lambda (x) (cons (car ls) x)) r1)
                r2)))))
  (iter ls n))


;; Return list of all possible combinations of list elements by given count.
;; Example: 
;;   (full-combinations '(a b c) 2) ->
;;   ((a a) (a b) (a c) (b b) (b c) (b a) (c c) (c a) (c b))
;;
(define (full-combinations ls n)

  ;; rotate elements of the list:
  ;; (rotate '(a b c)) -> (b c a)
  (define (rotate ls)
    (append (cdr ls) (list (car ls))))

  (if (= 1 n)
    (map list ls)
    (let loop ((len (length ls))
               (s ls)
               (res '()))
      (if (zero? len)
        res
        (loop (1- len) 
              (rotate s) 
              (append res 
                      (map (lambda (x) 
                             (cons (car s) x))
                           (full-combinations s (1- n)))))))))


;; end of file
