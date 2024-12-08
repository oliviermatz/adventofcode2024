(ns advent2024.day01
  (:require [advent2024.common :refer [read-resource-lines-extract]]))

(defn parse-lists []
  (let [pairs (->> (read-resource-lines-extract "day01.txt" #"(\d+)\s+(\d+)")
                   (map (fn [[a b]] [(parse-long a) (parse-long b)])))]
    [(map first pairs) (map second pairs)]))

(let [[a b] (parse-lists)
      [a-sorted b-sorted] [(sort a) (sort b)]
      diffs (map #(abs (- %1 %2)) a-sorted b-sorted)]
  (reduce + diffs))

(let [[a b] (parse-lists)
      b-occurences (into {} (map (fn [[k ks]] [k (count ks)])
                                 (group-by identity b)))]
  (reduce (fn [score a]
            (+ score (* a (or (b-occurences a) 0))))
          0
          a))

