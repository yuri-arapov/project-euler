;; 2010-09-02
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=109
;;
;; Problem 109
;;
;; 18 November 2005
;;
;; In the game of darts a player throws three darts at a target board which is
;; split into twenty equal sized sections numbered one to twenty.
;;
;; The score of a dart is determined by the number of the region that the dart
;; lands in. A dart landing outside the red/green outer ring scores zero. The
;; black and cream regions inside this ring represent single scores. However,
;; the red/green outer ring and middle ring score double and treble scores
;; respectively.
;;
;; At the centre of the board are two concentric circles called the bull
;; region, or bulls-eye. The outer bull is worth 25 points and the inner bull
;; is a double, worth 50 points.
;;
;; There are many variations of rules but in the most popular game the players
;; will begin with a score 301 or 501 and the first player to reduce their
;; running total to zero is a winner. However, it is normal to play a "doubles
;; out" system, which means that the player must land a double (including the
;; double bulls-eye at the centre of the board) on their final dart to win; any
;; other dart that would reduce their running total to one or lower means the
;; score for that set of three darts is "bust".
;;
;; When a player is able to finish on their current score it is called a
;; "checkout" and the highest checkout is 170: T20 T20 D25 (two treble 20s and
;; double bull).
;;
;; There are exactly eleven distinct ways to checkout on a score of 6:
;;
;;       D3   
;;       D1     D2       
;;       S2     D2       
;;       D2     D1       
;;       S4     D1       
;;       S1     S1      D2
;;       S1     T1      D1
;;       S1     S3      D1
;;       D1     D1      D1
;;       D1     S2      D1
;;       S2     S2      D1
;;
;; Note that D1 D2 is considered different to D2 D1 as they finish on different
;; doubles. However, the combination S1 T1 D1 is considered the same as T1 S1
;; D1.
;;
;; In addition we shall not include misses in considering combinations; for
;; example, D3 is the same as 0 D3 and 0 0 D3.
;;
;; Incredibly there are 42336 distinct ways of checking out in total.
;;
;; How many distinct ways can a player checkout with a score less than 100?
;;
;; Answer: 38182
;;
;; NOTE: p109-2 is much faster



(load "product.scm")


;; Debugging routine
(define (print s) (for-each (lambda (i) (format #t "~a\n" i)) s))


;; List of all possible shots
(define all-shots '(
S1 S2 S3 S4 S5 S6 S7 S8 S9 S10 S11 S12 S13 S14 S15 S16 S17 S18 S19 S20 S25
D1 D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 D12 D13 D14 D15 D16 D17 D18 D19 D20 D25
T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20
))


;; List of all possible doubles
(define double-shots '(
D1 D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 D12 D13 D14 D15 D16 D17 D18 D19 D20 D25
))


;; Determine value of the shot
(define (throw-value s)
  (case s
    ((S1)   1)   ((D1)   2)   ((T1)   3)
    ((S2)   2)   ((D2)   4)   ((T2)   6)
    ((S3)   3)   ((D3)   6)   ((T3)   9)
    ((S4)   4)   ((D4)   8)   ((T4)  12)
    ((S5)   5)   ((D5)  10)   ((T5)  15)
    ((S6)   6)   ((D6)  12)   ((T6)  18)
    ((S7)   7)   ((D7)  14)   ((T7)  21)
    ((S8)   8)   ((D8)  16)   ((T8)  24)
    ((S9)   9)   ((D9)  18)   ((T9)  27)
    ((S10) 10)   ((D10) 20)   ((T10) 30)
    ((S11) 11)   ((D11) 22)   ((T11) 33)
    ((S12) 12)   ((D12) 24)   ((T12) 36)
    ((S13) 13)   ((D13) 26)   ((T13) 39)
    ((S14) 14)   ((D14) 28)   ((T14) 42)
    ((S15) 15)   ((D15) 30)   ((T15) 45)
    ((S16) 16)   ((D16) 32)   ((T16) 48)
    ((S17) 17)   ((D17) 34)   ((T17) 51)
    ((S18) 18)   ((D18) 36)   ((T18) 54)
    ((S19) 19)   ((D19) 38)   ((T19) 57)
    ((S20) 20)   ((D20) 40)   ((T20) 60)
    ((S25) 25)   ((D25) 50)
    (else
      (error "bad shot" s))))


;; Determine unique tag (aka index) of the shot
(define (throw-tag s)
  (case s
    ((S1)   1)   ((D1)  31)   ((T1)  61)
    ((S2)   2)   ((D2)  32)   ((T2)  62)
    ((S3)   3)   ((D3)  33)   ((T3)  63)
    ((S4)   4)   ((D4)  34)   ((T4)  64)
    ((S5)   5)   ((D5)  35)   ((T5)  65)
    ((S6)   6)   ((D6)  36)   ((T6)  66)
    ((S7)   7)   ((D7)  37)   ((T7)  67)
    ((S8)   8)   ((D8)  38)   ((T8)  68)
    ((S9)   9)   ((D9)  39)   ((T9)  69)
    ((S10) 10)   ((D10) 40)   ((T10) 70)
    ((S11) 11)   ((D11) 41)   ((T11) 71)
    ((S12) 12)   ((D12) 42)   ((T12) 72)
    ((S13) 13)   ((D13) 43)   ((T13) 73)
    ((S14) 14)   ((D14) 44)   ((T14) 74)
    ((S15) 15)   ((D15) 45)   ((T15) 75)
    ((S16) 16)   ((D16) 46)   ((T16) 76)
    ((S17) 17)   ((D17) 47)   ((T17) 77)
    ((S18) 18)   ((D18) 48)   ((T18) 78)
    ((S19) 19)   ((D19) 49)   ((T19) 79)
    ((S20) 20)   ((D20) 50)   ((T20) 80)
    ((S25) 21)   ((D25) 51)
    (else
      (error "bad shot" s))))


;; Sort throws in the shot
(define (sort-throws i)
  (cons (car i) 
        (sort (cdr i) 
              (lambda (x y) 
                (< (throw-tag x) (throw-tag y))))))


;; Sort shots
(define (sort-shots s)
  (sort s 
        (lambda (i j)
          (cond ((< (length i) (length j)) #t)
                ((> (length i) (length j)) #f)
                (else
                  (let loop ((i i)
                             (j j))
                    (cond ((null? i) 
                           #f)
                          ((< (throw-tag (car i)) (throw-tag (car j)))
                           #t)
                          ((> (throw-tag (car i)) (throw-tag (car j)))
                           #f)
                          (else
                            (loop (cdr i) (cdr j))))))))))


;; Shots equivalence predicate
(define (shots=? s1 s2)
  (let loop ((s1 s1)
             (s2 s2))
    (cond ((and (null? s1) (null? s2))
           #t)
          ((or (null? s1) (null? s2))
           #f)
          ((not (= (throw-tag (car s1)) (throw-tag (car s2))))
           #f)
          (else
            (loop (cdr s1) (cdr s2))))))


;; Delete duplicates from the list; s must be sorted
(define (undup s eq?)
  (if (null? s)
    '()
    (let loop ((src (cdr s))
               (dst (list (car s))))
      (cond ((null? src)
             (reverse dst))
            ((eq? (car dst) (car src))
             (loop (cdr src) dst))
            (else
              (loop (cdr src) (cons (car src) dst)))))))



;; Find list of shots that would check given N out.
;; Or return false.
(define (find-checkouts N shot-num)
  (if (> shot-num 3)
    #f
    (let loop ((s (if (= 1 shot-num) double-shots all-shots))
               (res '()))
      (if (null? s)
        res
        (let ((f (throw-value (car s))))
          (cond 
            ((= f N)
             (loop (cdr s) (cons (list (car s)) res)))
            ((> f N)
             (loop (cdr s) res))
            ((< f N)
             (let ((r (find-checkouts (- N f) (1+ shot-num))))
               (if r
                 (loop (cdr s) (append (map (lambda (i) (cons (car s) i)) r) res))
                 (loop (cdr s) res))))))))))



;; Return number of checkouts for given score N
(define (number-of-checkouts N)
  (length
    (undup
      (sort-shots
        (map sort-throws
             (find-checkouts N 1)))
      shots=?)))


;; Problem 109 (see also p109-2)
(define (p109)
  (apply +
         (map number-of-checkouts (iota 99 1))))


;; Make list of all possible pairs made of elements of list s.
;; Example:
;;   (make-all-pairs '(1 2 3)) -> ((3 3) (2 2) (2 3) (1 1) (1 2) (1 3))
(define (make-all-pairs s)
  (let loop ((s s)
             (res '()))
    (if (null? s)
      res
      (loop (cdr s) (append (map (lambda (i) (list (car s) i)) s) res)))))


;; Make list of all 3-throw shots that end with double
(define (make-all-3-shots)
  (product
    double-shots
    (make-all-pairs all-shots)))


;; Make list of all 2-throw shots that end with double
(define (make-all-2-shots)
  (product
    double-shots
    all-shots))


;; Make list of all 1-throw shots
(define (make-all-1-shots)
  (map list double-shots))


;; Problem 109, revisited.
;; Possible optimization: compute (throw-value ...) just once and
;; memoize it.
(define (p109-2)
  (length
    (filter (lambda (n) (< n 100))
            (map (lambda (i) (apply + (map throw-value i)))
                 (append (make-all-1-shots)
                         (make-all-2-shots)
                         (make-all-3-shots))))))

;; end of file
;; vim: ts=4 sw=4
