;; 2010-08-20
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=105
;;
;; Problem 105
;; 23 September 2005
;;
;; Let S(A) represent the sum of elements in set A of size n. We shall call it
;; a special sum set if for any two non-empty disjoint subsets, B and C, the
;; following properties are true:
;;
;;    1. S(B) ≠ S(C); that is, sums of subsets cannot be equal.
;;    2. If B contains more elements than C then S(B) > S(C).
;;
;; For example, {81, 88, 75, 42, 87, 84, 86, 65} is not a special sum set
;; because 65 + 87 + 88 = 75 + 81 + 84, whereas {157, 150, 164, 119, 79, 159,
;; 161, 139, 158} satisfies both rules for all possible subset pair
;; combinations and S(A) = 1286.
;;
;; Using sets.txt (right click and "Save Link/Target As..."), a 4K text file
;; with one-hundred sets containing seven to twelve elements (the two examples
;; given above are the first two sets in the file), identify all the special
;; sum sets, A(1), A(2), ..., A(k), and find the value of 
;; S(A(1)) + S(A(2)) + ... + S(A(k)).
;;
;; NOTE: This problem is related to problems 103 and 106.
;;
;; Answer: 73702


(load "read-file.scm")
(load "combinations.scm")


(define (print s) (for-each (lambda (i) (format #t "~a\n" i)) s))


;; Return list of all possible combinations of the elements from s.
;; (all-combinations '(1 2 3)) -> ((1) (2) (3) (1 2) (1 3) (2 3) (1 2 3))
;;
(define (all-combinations s)
  (let loop ((n 1)
             (res '()))
    (let ((c (combinations s n)))
      (if (null? c)
        res
        (loop (1+ n) (append res c))))))



;; Return list of all disjoint subsets pairs of given set s.
;;
(define (disjoint-subsets s)
  (if (null? (cddr s))
    (list (cons (list (car s)) (list (list (cadr s)))))
    (let* ((first (car s))
           (rest  (cdr s))
           (pairs (disjoint-subsets rest)))
      (append
        pairs
        (map
          (lambda (pair)
            (list (cons first (car pair)) (cadr pair)))
          pairs)
        (map
          (lambda (pair)
            (list (car pair) (cons first (cadr pair))))
          pairs)
        (map
          (lambda (subset)
            (list (list first) subset))
          (all-combinations rest))))))



;; Sum of set elements.
;;
(define (set-sum s) (apply + s))



;; Test set for being special sum set.
;;
(define (special-sum-set? s)
  (every
    (lambda (ss-pair)
      (let* ((tmp (sort ss-pair (lambda (x y) (< (length x) (length y)))))
             (ss1 (car tmp))
             (ss2 (cadr tmp))
             (ss1-sum (set-sum ss1))
             (ss2-sum (set-sum ss2)))
        (and
          (not (= ss1-sum ss2-sum))
          (or (= (length ss1) (length ss2)) (< ss1-sum ss2-sum)))))
    (disjoint-subsets (sort s <))))
  


(define (test1)
  (special-sum-set? '(81 88 75 42 87 84 86 65)))


(define (test2)
  (special-sum-set? '(157 150 164 119 79 159 161 139 158)))



;; Problem 105, brute force.
;;
(define (p105)
  (let ((sets
          (read-file-with
            "sets.txt"
            (lambda (line)
              (map string->number (string-split line #\,))))))
    (let loop ((sets sets)
               (index 0)
               (sum 0))
      (if (zero? (remainder index 5))
        (format #t "~a\n" index))
      (if (null? sets)
        sum
        (loop
          (cdr sets)
          (1+ index)
          (if (special-sum-set? (car sets))
            (+ sum (set-sum (car sets)))
            sum))))))

;; end of file
;; vim: ts=4 sw=4
