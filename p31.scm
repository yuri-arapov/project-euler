;; 05 March 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=31
;;
;; Problem 31
;; 22 November 2002
;;
;; In England the currency is made up of pound, £, and pence, p, and there are
;; eight coins in general circulation:
;;
;; 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
;;
;; It is possible to make £2 in the following way:
;;
;;   1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
;;
;;  How many different ways can £2 be made using any number of coins?
;;
;; Answer:
;;      73682
;;
;; FIXME: still don't understand...


;; 14-Feb-2008
;;
;; Structure and Interpretation of Computer Programs
;; 
;; count the change
;;
;; FIXME: honestly I still don't understand how it works


(define (count-change amount)
  (cc amount 8))


(define (cc amount kinds-of-coins)
;;  (display "cc ") (display kinds-of-coins) (newline)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))


(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 2)
        ((= kinds-of-coins 3) 5)
        ((= kinds-of-coins 4) 10)
        ((= kinds-of-coins 5) 20)
        ((= kinds-of-coins 6) 50)
        ((= kinds-of-coins 7) 100)
        ((= kinds-of-coins 8) 200)))


;; end of file
