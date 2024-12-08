(ns advent2024.day05
  (:require [advent2024.common :refer [read-resource-lines extract-regex-groups]]
            [clojure.string :refer [split]]))

(defn satisfy-rule [update rule]
  (let [l-i (.indexOf update (first rule))
        r-i (.indexOf update (second rule))]
    (or (< l-i 0) (< r-i 0) (< l-i r-i))))

(defn get-rules-and-updates []
  (let [[rules-str [_ & updates-str]] (split-with #(not= "" %) (read-resource-lines "day05.txt"))
        rules (map #(split % #"\|") rules-str)
        updates (map #(split % #",") updates-str)]
    [rules updates]))

(let [[rules updates] (get-rules-and-updates)
      correct-updates
      (filter (fn [update] (every? #(satisfy-rule update %) rules))
              updates)]
  (reduce + 0 (map #(parse-long (nth % (/ (count %) 2))) correct-updates)))

(let [[rules updates] (get-rules-and-updates)
      incorrect-updates
      (filter (fn [update] (not (every? #(satisfy-rule update %) rules)))
              updates)
      comp (fn [a b]
             (cond
               (some #(= % [a b]) rules) -1
               (some #(= % [b a]) rules) 1
               :else 0))
      corrected-updates (map #(sort comp %) incorrect-updates)]
  (reduce + 0 (map #(parse-long (nth % (/ (count %) 2))) corrected-updates)))

