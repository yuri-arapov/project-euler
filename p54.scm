;; 19 March 2009
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=54
;;
;; Problem 54
;;
;; 10 October 2003
;;
;; In the card game poker, a hand consists of five cards and are ranked, from
;; lowest to highest, in the following way:
;;
;;     * High Card       : Highest value card.
;;     * One Pair        : Two cards of the same value.
;;     * Two Pairs       : Two different pairs.
;;     * Three of a Kind : Three cards of the same value.
;;     * Straight        : All cards are consecutive values.
;;     * Flush           : All cards of the same suit.
;;     * Full House      : Three of a kind and a pair.
;;     * Four of a Kind  : Four cards of the same value.
;;     * Straight Flush  : All cards are consecutive values of same suit.
;;     * Royal Flush     : Ten, Jack, Queen, King, Ace, in same suit.
;;
;; The cards are valued in the order:
;; 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
;;
;; If two players have the same ranked hands then the rank made up of the
;; highest value wins; for example, a pair of eights beats a pair of fives (see
;; example 1 below). But if two ranks tie, for example, both players have a
;; pair of queens, then highest cards in each hand are compared (see example 4
;; below); if the highest cards tie then the next highest cards are compared,
;; and so on.
;;
;; Consider the following five hands dealt to two players:
;;
;; Hand            Player 1                Player 2                Winner
;; 1               5H 5C 6S 7S KD          2C 3S 8S 8D TD          Player 2
;;                 Pair of Fives           Pair of Eights
;;
;; 2               5D 8C 9S JS AC          2C 5C 7D 8S QH          Player 1
;;                 Highest card Ace        Highest card Queen
;;
;; 3               2D 9C AS AH AC          3D 6D 7D TD QD          Player 2
;;                 Three Aces              Flush with Diamonds
;;
;; 4               4D 6S 9H QH QC          3D 6D 7H QD QS          Player 1
;;                 Pair of Queens          Pair of Queens
;;                 Highest card Nine       Highest card Seven
;;
;; 5               2H 2D 4C 4D 4S          3C 3D 3S 9S 9D          Player 1
;;                 Full House              Full House
;;                 With Three Fours        with Three Threes
;;
;;
;; The file, poker.txt, contains one-thousand random hands dealt to two
;; players. Each line of the file contains ten cards (separated by a single
;; space): the first five are Player 1's cards and the last five are Player 2's
;; cards. You can assume that all hands are valid (no invalid characters or
;; repeated cards), each player's hand is in no specific order, and in each
;; hand there is a clear winner.
;;
;; How many hands does Player 1 win?
;;
;; Answer: 376



;;
;; Engage 'receive'.
;;
(use-modules (ice-9 receive))


;;
;; Format stub.
;;
(define (format-stub dest fmt . args) #t)


;;
;; Shadow system format.
;;
(define format format-stub)


;;
;; Simple listing.
;;
(define (ll x) (for-each (lambda (y) (format #t "~a\n" y)) x))


;;
;; Split strings from list ls into hands of player 1 and 2
;; (five cards per hand).
;; Return two lists (via 'values'): first list are the hands of player 1,
;; and second list are the hands of player 2.
;;
(define (split-hands ls)
  (define (iter player-1 player-2 ls)
    (if (null? ls)
      (values (reverse player-1)
              (reverse player-2))
      (let* ((l  (string-split (car ls) #\ ))
             (h1 (take l 5))    ;; first 5 cards
             (h2 (drop l 5)))   ;; last 5 cards
        (iter (cons h1 player-1) (cons h2 player-2) (cdr ls)))))
  (iter '() '() ls))


;;
;; Sort the cards in descending order (higher to lower).
;;
(define (sort-cards cards)
  (sort cards (lambda (c1 c2) (> (card-value-num c1) (card-value-num c2)))))


;;
;; Return true if given cards match given pattern.
;; Cards is list of cards, for example (9C 8S 7D 6C 5H),
;; pattern is string, for example "xx---".
;; The same characters in the pattern imply that there must
;; be cards of the same values correspondingly 
;; (except for '-' which matches any card).
;;
(define (match-pattern? cards pattern)
  (define (iter c c-prev p p-prev)
    (cond ((or (null? c) (null? p))     ;; end of pattern or end of card:
           #t)                          ;;   match found

          ((eq? #\- (car p))            ;; '-' in the pattern
             (iter (cdr c) #\- (cdr p) #\-))

          ((eq? (car p) p-prev)         ;; same character in the pattern
           (if (eq? (car c) c-prev)
             (iter (cdr c) c-prev (cdr p) p-prev)
             #f))

          (else                         ;; different character in the pattern
            (if (eq? (car c) c-prev)
              #f
              (iter (cdr c) (car c) (cdr p) (car p))))))

  (iter (map card-value cards)  ;; turn cards into list of value characters
        #\-
        (string->list pattern)  ;; turn pattern into list of characters
        #\-))
           

;;
;; Card accessors.
;;
(define (card-value     card) (string-ref card 0))
(define (card-suit      card) (string-ref card 1))
(define (card-value-num card) (+ 2 (string-index "23456789TJQKA" (card-value card))))


;;
;; The Ace.
;;
(define Ace #\A)


;;
;; Return true if all the cards are the same suit.
;;
(define (flush? cards)
  (let ((s (card-suit (first cards))))
    (every (lambda (c) (eq? s (card-suit c))) cards)))


;;
;; Return true if it's a straight.
;; Cards must be sorted in descending order (higher to lower).
;;
(define (straight? cards)
  (define (<> x y) (not (= x y)))

  (cond ((or (null? cards) (null? (cdr cards)))
         #t)    ;; less than two items in the list

        ((<> 1 (- (card-value-num (first cards)) 
                  (card-value-num (second cards))))
         #f)    ;; not a consecutive values in the pair

        (else
          (straight? (cdr cards)))))    ;; check next pair


(define (straight-flush? cards)
  (and (straight? cards) (flush? cards)))


(define (royal-flush? cards)
  (and (eq? Ace (card-value (car cards))) 
       (straight-flush? cards)))


(define (match-any-pattern? cards patterns)
  (any (lambda (p) (match-pattern? cards p)) patterns))


(define (full-house? cards)      (match-any-pattern? cards '("xxyyy" "xxxyy")))
(define (four-of-a-kind? cards)  (match-any-pattern? cards '("xxxx-" "-xxxx")))
(define (three-of-a-kind? cards) (match-any-pattern? cards '("xxx--" "-xxx-" "--xxx")))
(define (two-pairs? cards)       (match-any-pattern? cards '("xxyy-" "xx-yy" "-xxyy")))
(define (one-pair? cards)        (match-any-pattern? cards '("xx---" "-xx--" "--xx-" "---xx")))


;;
;; Return true if player 1 wins with higher card.
;;
(define (p1-high-card? p1 p2)
  (let ((c1 (if (null? p1) #f (card-value-num (car p1))))
        (c2 (if (null? p2) #f (card-value-num (car p2)))))
    (cond ((or (not c1) (not c2))   #f)
          ((= c1 c2)                (p1-high-card? (cdr p1) (cdr p2)))
          (else                     (> c1 c2)))))


;;
;; Possible ranks, highest first.
;;
(define rank (list royal-flush? 
                   four-of-a-kind?
                   full-house?
                   flush?
                   straight?
                   three-of-a-kind?
                   two-pairs?
                   one-pair?))


;;
;; Return value of the pair.  
;; Return false if cards don't contain pair.
;; E.g. for cards (JS 6S 5H 4S 4C) it'll return 4.
;;
(define (pair-value cards)
  (cond ((or (null? cards) (null? (cdr cards)))
         #f)    ;; less than two cards left

        ((eq? (card-value-num (first cards)) 
              (card-value-num (second cards)))
         (card-value-num (first cards)))    ;; result found

        (else
          (pair-value (cdr cards)))))       ;; try next pair


;;
;; Return higher card when hand contains a pair with value p.
;;
(define (pair-high-card cards p)
  (if (null? cards)
    #f
    (if (eq? p (card-value-num (car cards)))
      (pair-high-card (cdr cards) p)
      (card-value-num (car cards)))))


;;
;; Return true if player1 wins when both players have equals pairs
;; and player 1 has higher card.
;;
(define (p1-high-pair? p1 p2)
  (let ((v1 (pair-value p1))
        (v2 (pair-value p2)))
    (if (not (eq? v1 v2))
      (> v1 v2)
      (let ((vv1 (pair-high-card p1 v1))
            (vv2 (pair-high-card p2 v1)))
        (format #t "~a ~a " vv1 vv2)    ;; for debugging
        (if (not (eq? vv1 vv2))
          (> vv1 vv2)
          (error "same pairs"))))))


;;
;; All the magic is here.
;;
(define (xx p1 p2)
  (define (iter p1 p2 r n)

    (let ((c1 (if (null? p1) #f (car p1)))  ;; cards of player 1
          (c2 (if (null? p2) #f (car p2)))  ;; cards of player 2
          (t  (if (null? r)  #f (car r))))  ;; test for the given rank

      (cond ((not c1)   ;; no cards any more, return result
             n)

            ((not t)    ;; all ranks have been tried, try higer card now
             (let ((p1-wins (p1-high-card? c1 c2)))
               (format #t "~a wins: ~a ~a high card\n" (if p1-wins "p1" "p2") c1 c2)
               (iter (cdr p1) (cdr p2) rank (if p1-wins (+ n 1) n))))

            (else
              (let ((m1 (t c1))     ;; true if cards of player 1 match the rank
                    (m2 (t c2)))    ;; true if cards of player 2 match the rank
                (cond ((and m1 (not m2))    ;; p1 wins
                       (format #t "p1 wins: ~a ~a ~a\n" c1 c2 t)
                       (iter (cdr p1) (cdr p2) rank (+ n 1)))

                      ((and m2 (not m1))    ;; p2 wins
                       (format #t "p2 wins: ~a ~a ~a\n" c1 c2 t)
                       (iter (cdr p1) (cdr p2) rank n))

                      ((and (not m1) (not m2))  ;; both don't match
                       (iter p1 p2 (cdr r) n))

                      (else ;; both match
                        (if (not (one-pair? c1))
                          (error "not one pair")
                          (let ((p1-wins (p1-high-pair? c1 c2)))
                            (format #t "~a wins: ~a ~a high pair\n" (if p1-wins "p1" "p2") c1 c2)
                            (iter (cdr p1) (cdr p2) rank (if p1-wins (+ n 1) n)))))))))))

  (iter p1 p2 rank 0))


;;
;; Read poker file, sort cards in each hand and do all the magic.
;;
(define (p54)
  (receive (p1 p2) (split-hands (read-file "poker.txt"))
    (let ((pp1 (map sort-cards p1))
          (pp2 (map sort-cards p2)))
      (xx pp1 pp2))))


;; end of file
;; vim: ts=4 sw=4
