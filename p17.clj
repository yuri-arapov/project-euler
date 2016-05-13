;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=17
;;
;; Problem 17
;; 17 May 2002
;;
;; If the numbers 1 to 5 are written out in words: one, two, three, four, five;
;; there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
;;
;; If all the numbers from 1 to 1000 (one thousand) inclusive were written out
;; in words, how many letters would be used?
;;
;; NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
;; forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
;; letters.
;;
;; Answer:
;;      21124


;; text for numbers from 1 to 20
(def one-twenty
  [""
   "one"
   "two"
   "three"
   "four"
   "five"
   "six"
   "seven"
   "eight"
   "nine"
   "ten"
   "eleven"
   "twelve"
   "thirteen"
   "fourteen"
   "fifteen"
   "sixteen"
   "seventeen"
   "eighteen"
   "nineteen"
   "twenty"])


;; text for tens
(def twenty-ninety
  [""
   "ten"
   "twenty"
   "thirty"
   "forty"
   "fifty"
   "sixty"
   "seventy"
   "eighty"
   "ninety"])


;; compute n^p
(defn pow [n p] (apply * (repeat p n)))


;; return pos decimal digit of number n counting from right:
;; (digit 123 1) => 3
(defn digit [n pos] (rem (quot n (pow 10 (dec pos))) 10))


;; convert number [0..99] to text
(defn nn->text [nn]
  (cond (<= 0 nn 20) (get one-twenty nn)
        (zero? (digit nn 1)) (get twenty-ninety (digit nn 2))
        :else (str (get twenty-ninety (digit nn 2)) "-" (get one-twenty (digit nn 1)))))


;; get text for thousands and hundreds
(defn pos->text [n pos suffix]
  (let [d (digit n pos)]
    (if (pos? d)
      (str (get one-twenty d) " " suffix)
      "")))


;; convert number [1..1000] to text
(defn number->text [n]
  (reduce
   (fn [res [sep e]]
     (cond (and (empty? res) (empty? e)) res
           (empty? e) res
           (empty? res) e
           :else (str res sep e)))
   ""
   [[""  (pos->text n 4 "thousand")]
    [" " (pos->text n 3 "hundred")]
    [" and " (nn->text (rem n 100))]]))


(defn p17 []
  (apply + (map (fn [n] (count (filter #(Character/isLetter %) (number->text n))))
                (range 1 1001))))

;;
