;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=42
;;
;; Problem 42
;; 25 April 2003
;;
;; The nth term of the sequence of triangle numbers is given by, 
;;
;;        n(n+1)
;;   tn = ------
;;          2
;;
;; so the first ten triangle numbers are:
;;
;; 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
;;
;; By converting each letter in a word to a number corresponding to its
;; alphabetical position and adding these values we form a word value. For
;; example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word
;; value is a triangle number then we shall call the word a triangle word.
;;
;; Using words.txt (right click and 'Save Link/Target As...'), a 16K text file
;; containing nearly two-thousand common English words, how many are triangle
;; words?
;;
;; Answer: 162

(defn word->number [w]
  (apply + (map (fn [c] (- (int c) 64)) w)))

(defn triangular-number? [x]
  (let [n (/ (- (Math/sqrt (+ (* 8 x) 1)) 1) 2)]
    (= (Math/floor n) (Math/ceil n))))

(defn triangular-word? [w]
  (triangular-number? (word->number w)))

(defn p42 []
  (->> "words"                      ;; file name
       slurp                        ;; read it as single string
       clojure.string/split-lines   ;; split it into lines
       (filter triangular-word?)    ;; remove non-triangular words
       count))                      ;; count triangular words

;; end of file
