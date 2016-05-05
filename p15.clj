;; Problem Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=15
;;
;; Problem 15
;; 19 April 2002
;;
;; Starting in the top left corner of a 2×2 grid, there are 6 routes (without
;; backtracking) to the bottom right corner.
;;
;; How many routes are there through a 20×20 grid?
;; Answer:
;;      137846528820

(defn p15
  ([rows cols]
   (let [grid (atom {})
         grid-value (fn [r c] (if-let [[[_ _] x] (find @grid [r c])] x))
         grid-value! (fn [r c v] (swap! grid assoc [r c] v))]
     (letfn [(R [r c]
               (cond (grid-value r c) :nothing
                     (= r 1) (grid-value! r c (+ c 1))
                     (= c 1) (grid-value! r c (+ r 1))
                     :else (grid-value! r c (+ (R (dec r) c) (R r (dec c)))))
               (grid-value r c))]
       (R rows cols))))
  ([]
   (p15 20 20)))


;; end of file
