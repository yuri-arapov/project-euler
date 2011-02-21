;; 30 Jun. 2010
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=93
;;
;; Problem 93
;; 15 April 2005
;;
;; By using each of the digits from the set, {1, 2, 3, 4}, exactly once, and
;; making use of the four arithmetic operations (+, −, *, /) and
;; brackets/parentheses, it is possible to form different positive integer
;; targets.
;;
;; For example,
;;
;; 8 = (4 * (1 + 3)) / 2
;; 14 = 4 * (3 + 1 / 2)
;; 19 = 4 * (2 + 3) − 1
;; 36 = 3 * 4 * (2 + 1)
;;
;; Note that concatenations of the digits, like 12 + 34, are not allowed.
;;
;; Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one different
;; target numbers of which 36 is the maximum, and each of the numbers 1 to 28
;; can be obtained before encountering the first non-expressible number.
;;
;; Find the set of four distinct digits, a < b < c < d, for which the longest
;; set of consecutive positive integers, 1 to n, can be obtained, giving your
;; answer as a string: abcd.
;;
;; Answer: 1258 (51)
;;


(load "permutations.scm")
(load "combinations.scm")
(load "uniq.scm")
(load "product.scm")


;; Make a function that checks its arguments for not being #f
;; before computation.
;;
(define (mk-op op)
  (lambda (x y)
    (if (and x y)
      (op x y)
      #f)))


(define (safe-div x y)
  (if (zero? y)
    #f
    (/ x y)))


(define add (mk-op +))
(define sub (mk-op -))
(define mul (mk-op *))
(define div (mk-op safe-div))


;; Reuturn priority of the operation.
;;
(define (prio op)
  (if (or (equal? op add)
          (equal? op sub)
          (equal? op +)
          (equal? op -))
    0
    1))


;; Apply given set of operations ops on some of c? functions
;; and return function that expects four agruments.
;; 
(define (make-c c-fn ops)
  (lambda (a1 a2 a3 a4)
    (c-fn a1 (car ops) a2 (cadr ops) a3 (caddr ops) a4)))

;; a1 o1 a2 o2 a3 o3 a4
(define (c1 a1 o1 a2 o2 a3 o3 a4)
  (compute a1 o1 a2 o2 a3 o3 a4))

;; (a1 o1 a2) o2 a3 o3 a4
(define (c2 a1 o1 a2 o2 a3 o3 a4)
  (compute (compute a1 o1 a2) o2 a3 o3 a4))

;; a1 o1 (a2 o2 a3) o3 a4
(define (c3 a1 o1 a2 o2 a3 o3 a4)
  (compute a1 o1 (compute a2 o2 a3) o3 a4))

;; a1 o1 a2 o2 (a3 o3 a4)
(define (c4 a1 o1 a2 o2 a3 o3 a4)
  (compute a1 o1 a2 o2 (compute a3 o3 a4)))

;; (a1 o1 a2) o2 (a3 o3 a4)
(define (c5 a1 o1 a2 o2 a3 o3 a4)
  (compute (compute a1 o1 a2) o2 (compute a3 o3 a4)))

;; (a1 o1 a2 o2 a3) o3 a4
(define (c6 a1 o1 a2 o2 a3 o3 a4)
  (compute (compute a1 o1 a2 o2 a3) o3 a4))

;; a1 o1 (a2 o2 a3 o3 a4)
(define (c7 a1 o1 a2 o2 a3 o3 a4)
  (compute a1 o1 (compute a2 o2 a3 o3 a4)))

;; ((a1 o1 a2) o2 a3) o3 a4
(define (c8 a1 o1 a2 o2 a3 o3 a4)
  (compute (compute (compute a1 o1 a2) o2 a3) o3 a4))

;; (a1 o1 (a2 o2 a3)) o3 a4
(define (c9 a1 o1 a2 o2 a3 o3 a4)
  (compute (compute a1 o1 (compute a2 o2 a3)) o3 a4))

;; a1 o1 ((a2 o2 a3) o3 a4)
(define (c10 a1 o1 a2 o2 a3 o3 a4)
  (compute a1 o1 (compute (compute a2 o2 a3) o3 a4)))

;; a1 o1 (a2 o2 (a3 o3 a4))
(define (c11 a1 o1 a2 o2 a3 o3 a4)
  (compute a1 o1 (compute a2 o2 (compute a3 o3 a4))))

;; (compute) -> #f
;; (compute arg) -> arg
;; (compute a1 o1 a2 ...) -> apply first operations of priority 1,
;;                           then apply operations of priority 0.
;;
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


;; Return max continuous number in the list starting from 1.
;; Example:
;;   (max-cont-num '(1 2 3 4 6) -> 4
;;
(define (max-cont-num s)
  (let loop ((n 1)
             (s s))
    (if (or (null? s)
            (not (= n (car s))))
      (1- n)
      (loop (1+ n) (cdr s)))))


;; All possible combinations of 4 operations by 3.
;;
(define *ops (full-combinations (list add sub mul div) 3))


;; All possible combinations of c? functions applied with
;; possible combinations of operations.
;;
(define *c-funcs 
  (map (lambda (x) (make-c (car x) (cdr x)))
       (product (list c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11) *ops)))


;;
;; octet is a '(1 2 3 4) -like list.
;;
(define (p93-o octet)
  (max-cont-num
    (uniq
      (sort 
        (filter (lambda (n) (and (integer? n) (positive? n)))
                (each-to-each 
                  (lambda (c o) (apply c o))
                  *c-funcs 
                  (permutations octet)))
        <))))


(define (p93)
  (let loop ((s         (combinations (iota 10) 4))
             (max-octet '())
             (max-n     0))
    (cond ((null? s)
           (list max-n max-octet))
          (else
            (let* ((o (car s))
                   (n (p93-o o)))
              (cond ((> n max-n)
                     (format #t "~a -> ~a\n" o n)
                     (loop (cdr s) o n))
                    (else
                      (loop (cdr s) max-octet max-n))))))))

;; end of file
;; vim: ts=4 sw=4
