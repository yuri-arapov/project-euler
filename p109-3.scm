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
;; NOTE: the rule of KISS applied



(load "product.scm")


;; List of all possible shots
(define all-shots '(
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 25
2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 50
3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60
))


;; List of all possible doubles
(define double-shots '(
2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 50
))


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


;; Problem 109
(define (p109)
  (length
    (filter (lambda (n) (< n 100))
            (map (lambda (i) (apply + i))
                 (append (make-all-1-shots)
                         (make-all-2-shots)
                         (make-all-3-shots))))))

;; end of file
;; vim: ts=4 sw=4
