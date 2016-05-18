;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=22
;;
;; Problem 22
;; 19 July 2002
;;
;; Using names.txt (right click and 'Save Link/Target As...'), a 46K text file
;; containing over five-thousand first names, begin by sorting it into
;; alphabetical order. Then working out the alphabetical value for each name,
;; multiply this value by its alphabetical position in the list to obtain a name
;; score.
;;
;; For example, when the list is sorted into alphabetical order, COLIN, which is
;; worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
;; would obtain a score of 938 x 53 = 49714.
;;
;; What is the total of all the name scores in the file?
;;
;; Answer:
;;      871198282


;; compute value of the character.
;; the 'A' (ascii code 65) value is 1.
(defn char->int [c] (- (int c) 64))

(defn name->value [name]
  (apply + (map char->int name)))

(defn read-names-1 [fname]
  (with-open [rdr (clojure.java.io/reader fname)]
    (loop [res [], lines (line-seq rdr)]
      (if (empty? lines) res
          (recur (conj res (first lines)) (rest lines))))))

(defn read-names [fname]
  (-> fname slurp (clojure.string/replace "\"" "") (clojure.string/split #",") sort))

(defn p22 []
  (reduce
   (fn [[res pos] name] [(+ res (* pos (name->value name))) (inc pos)])
   [0 1]
   (read-names "names.txt")))

;; end of file
