
(defn read-file [fname valid-line? accum init]
  "[fname valid-line? accum init]"
  (with-open [rdr (clojure.java.io/reader fname)]
    (loop [res init, [line & more-lines :as input] (line-seq rdr)]
      (cond (empty? input) res
            (not (valid-line? line)) false
            :else (recur (accum res line) more-lines)))))


