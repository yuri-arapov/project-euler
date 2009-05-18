;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=62
;;
;; Problem 62
;; 30 January 2004
;;
;; The cube, 41063625 (345^(3)), can be permuted to produce two other cubes:
;; 56623104 (384^(3)) and 66430125 (405^(3)). In fact, 41063625 is the smallest
;; cube which has exactly three permutations of its digits which are also cube.
;;
;; Find the smallest cube for which exactly five permutations of its digits are
;; cube.
;;
;; Answer: 127035954683


(load "range.scm")


(define (ll l)
  (for-each (lambda (i) (format #t "~a\n" i)) l))


;;
;; Return distinct elements of sorted list.
;;
;; Example:
;;   (unique '(1 1 1 2 2 2 3 3 3 3 3 4))
;;   (1 2 3 4)
;;
(define (unique l)
  (let loop ((l l)
             (r '()))
    (cond ((null? l)                (reverse r))
          ((null? (cdr l))          (loop (cdr l) (cons (car l) r)))
          ((eq? (car l) (cadr l))   (loop (cdr l) r))
          (else                     (loop (cdr l) (cons (car l) r))))))


;;
;; Pack series of equal elements into sublists.
;;
;; Example:
;;   (join-equal-elements '(1 1 1 1 2 2 2 2 2 2 2 2 3 3 4) =)
;;   ((1 1 1 1) (2 2 2 2 2 2 2 2) (3 3) (4))
;;
(define (join-equal-elements ls equal-elements?)
  (if (null? ls)
    '()

    (let loop ((in  (cdr ls))
               (tmp (list (car ls)))
               (out '()))

      (cond ((null? in)
             (reverse (cons tmp out)))

            ((equal-elements? (car in) (car tmp))
             (loop (cdr in) 
                   (reverse (cons (car in) tmp))
                   out))

            (else
              (loop (cdr in) 
                    (list (car in))
                    (cons tmp out)))))))


(define (number->digits n)
  (let loop ((n n)
             (l '()))
    (let ((q (quotient n 10)))
      (if (zero? q)
        (cons n l)
        (loop q (cons (remainder n 10) l))))))


(define (digits->number dd)
  (reduce (lambda (d res) (+ (* res 10) d)) 0 dd))


(define zero-char-value (char->integer #\0))


(define (ff ls)
  (map (lambda (n)
         (let* ((nnn (* n n n))
                (ddd-sorted (sort-list (number->digits nnn) <))
                (nnn-str (list->string
                           (map (lambda (d) (integer->char (+ d zero-char-value)))
                                ddd-sorted))))
           (list n nnn nnn-str)))
       ls))


(define (fff len ls)
  (let* ((a (ff ls))
         (b (sort-list a (lambda (x y) (string-ci< (third x) (third y)))))
         (c (join-equal-elements b (lambda (x y) (string-ci=? (third x) (third y)))))
         (d (filter (lambda (i) (= len (length i))) c))
         (e (map (lambda (i) (sort-list i (lambda (x y) (< (car x) (car y))))) d))
         (f (sort-list e (lambda (x y) (< (caar x) (caar y))))))
    (ll (car f))))


(format #t "hint: (fff 5 (range 111 9999))\n")


;; end of file
;; vim: ts=4 sw=4
