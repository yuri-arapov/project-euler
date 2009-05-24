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
;;
;; Changelog:
;;   * Performance improvements by Goedel encoding, see
;;     http://projecteuler.net/index.php?section=forum&id=62&page=4


(load "range.scm")
(load "ll.scm")


;;
;; Encode digits as Goedel number.
;;
(define (goedel-number digits)
  (apply * (map (lambda (d) 
                  (vector-ref '#(2 3 5 7 11 13 17 19 23 29) d)) 
                digits)))


;;
;; Return distinct elements of sorted list.
;;
;; Example:
;;   (unique '(1 1 1 2 2 2 3 3 3 3 3 4))
;;   (1 2 3 4)
;;
;; Note: not used.
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
                   (cons (car in) tmp)
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


;;
;; First step of data transformation: turn each number n into tuple
;; containing (n, n^3, n^3-goedel-encoded).
;;
(define (ff ls)
  (map (lambda (n)
         (let* ((nnn (* n n n))
                (ddd-sorted (sort-list (number->digits nnn) <))
                (nnn-goedel (goedel-number ddd-sorted)))
           (list n nnn nnn-goedel)))
       ls))


;;
;; Main program.
;;
;; len is a number of cube permutations we're interested in.
;; ls is a list of consecutive numbers.
;;
(define (fff len ls)
  (let* ((a (ff ls))
         ;; map list ls: n -> tuple: (n n^3 n^3-as-goedel-number)

         (b (sort-list a (lambda (x y) (< (third x) (third y)))))
         ;; sort list by last tuple member: n^3-as-sorted-digits

         (c (join-equal-elements b (lambda (x y) (= (third x) (third y)))))
         ;; collect equal list elements

         (d (filter (lambda (i) (= len (length i))) c))
         ;; collect chains of given length

         (e (map (lambda (i) (sort-list i (lambda (x y) (< (car x) (car y))))) d))
         ;; sort every chain by first tuple member: n

         (f (sort-list e (lambda (x y) (< (caar x) (caar y))))))
         ;; sort chains by first tuple member of first chain element

    (ll (car f))))


(format #t "hint: (fff 5 (range 111 9999))\n")


;; end of file
;; vim: ts=4 sw=4
