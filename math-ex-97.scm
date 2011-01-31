;; Misha's math workbook
;; Exercise 97
;;


(load "combinations.scm")
(load "product.scm")


;; Make a function that checks its arguments for not being #f
;; before computation.
(define (mk-op op)
  (lambda (x y)
    (if (and x y)
      (op x y)
      #f)))


;; Zero-safe division.
(define (safe-div x y)
  (if (zero? y)
    #f
    (/ x y)))


(define add (mk-op +))
(define sub (mk-op -))
(define mul (mk-op *))
(define div (mk-op safe-div))


;; Return priority of the operation.
(define (prio op)
  (if (or (equal? op add)
          (equal? op sub)
          (equal? op +)
          (equal? op -))
    0
    1))


;; Return string that represents give operation.
(define (op->string op)
  (let loop ((x (list add "+" 
                      sub "-" 
                      mul "*" 
                      div "/")))
    (if (null? x)
      "not-found"
      (if (equal? op (car x))
        (cadr x)
        (loop (cddr x))))))


;; (compute) -> #f
;; (compute arg) -> arg
;; (compute a1 o1 a2 ...) -> apply first operations of priority 1,
;;                           then apply operations of priority 0.
(define (compute . args)
  (define (compute-prio p args)
    (if (null? (cdr args))
      args
      (let loop ((head '())             ;; args/ops already passed
                 (la   (car args))      ;; left operand
                 (op   (cadr args))     ;; operation
                 (ra   (caddr args))    ;; right operand
                 (tail (cdddr args)))   ;; args/ops not yet processed
        (if (= p (prio op))
          (let ((res (op la ra)))
            (if (null? tail)
              (append head (list res))
              (loop 
                head            ;; same head
                res             ;; new left operand
                (car tail)      ;; new op
                (cadr tail)     ;; new right operand
                (cddr tail))))  ;; new tail
          (if (null? tail)
            (append head (list la op ra))
            (loop 
              (append head (list la op))    ;; new head
              ra                            ;; right operand now becomes left one
              (car tail)                    ;; new operation
              (cadr tail)                   ;; new right operand
              (cddr tail)))))))             ;; new tail
  (if (null? args)
    #f
    (car (compute-prio 0 (compute-prio 1 args)))))


;; Example:
;;   (mk-compute-args '(1 2 3) '(+ -)) -> (1 + 2 - 3)
(define (mk-compute-args args ops)
  (let loop ((a (cdr args)) (o ops) (res (list (car args))))
    (if (null? a)
      (reverse res)
      (loop (cdr a) (cdr o) (cons (car a) (cons (car o) res))))))


;; All possible combinations of 4 operations by 2.
(define *ops* (full-combinations (list add sub mul div) 2))


;; Find math operators +,-,*,/ to make expression 
;;   a ? b ? c = e ? f ? g
;; be true.
;; Example:
;;   (ex97 63 9 28 7 4 7) -> print list of solutions
(define (ex97 a b c e f g)

  (define (do-compute args ops) 
    (apply compute (mk-compute-args args ops)))

  (define (print-compute args ops)
    (for-each (curry format #t "~a ")
              (mk-compute-args args (map op->string ops))))

  (let ((x (list a b c))
        (y (list e f g)))
    (for-each 
      (lambda (op-pair)
        (let ((ops1 (car op-pair))
              (ops2 (cadr op-pair)))
          (and-let* ((xx (do-compute x ops1))
                     (yy (do-compute y ops2))
                     ((= xx yy)))
            (print-compute x ops1)
            (format #t "= ")
            (print-compute y ops2)
            (format #t "= ~a~%" xx))))
      (product (map list *ops*) (map list *ops*))))) ;; (((+ +) (+ +)) ((+ +) (+ -))...)-like
                                                     ;; list: all possible combinations of
                                                     ;; operators for a ? b ? c = e ? g ? f
                                                     ;; equation.


;; end of file
;; vim: ts=4 sw=4
