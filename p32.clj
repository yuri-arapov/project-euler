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

;; 14-Feb-2008
;;
;; Structure and Interpretation of Computer Programs
;; 
;; count the change, page 54 (Russian edition)


(defn count-change [amount coins]
  ;;(println amount coins)
  (cond (zero? amount) 1
        (neg? amount) 0
        (empty? coins) 0
        :else (+ (count-change amount (rest coins))
                 (count-change (- amount (first coins)) coins))))

(defn p32 []
  (count-change 200 [1 2 5 10 20 50 100 200]))


;; the version with memoization does not give any improvements over original one
;; because arguments of (count-change ...) do not repeat.
(defn count-change-memo [amount coins]
  (let 
    [memo (atom {})
     memo-get (fn [amount coins] (get @memo [amount coins]))
     memo-set! (fn [amount coins value] (swap! memo assoc [amount coins] value) value)
     count-change 
     (fn [amount coins]
       (or (memo-get amount coins)
           (memo-set! 
             amount
             coins
             (cond (zero? amount) 1
                   (or (neg? amount) (empty? coins)) 0
                   :else 
                     (+ (count-change amount (rest coins))
                        (count-change (- amount (first coins)) coins))))))]
    (count-change amount coins)))

(defn p32-memo []
  (count-change-memo 200 [1 2 5 10 20 50 100 200]))


;; end of file
