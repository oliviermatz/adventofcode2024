(ns advent2024.day02
  (:require [advent2024.common :refer [read-resource-lines]]
            [clojure.string :refer [split]]))

(defn parse-reports []
  (->> (read-resource-lines "day02.txt")
       (map #(split % #" "))
       (map #(map parse-long %))))

(defn is-all-pairwise [levels pred]
  (->> (map (fn [a b]
              (pred a b))
            (butlast levels)
            (rest levels))
       (every? identity)))

(defn safe? [report]
  (and (or (is-all-pairwise report <)
           (is-all-pairwise report >))
       (is-all-pairwise report (fn [a b]
                                 (<= (abs (- a b)) 3)))))

(defn dampened-reports [report]
  (map
   (fn [i]
     (concat 
      (subvec report 0 (dec i))
      (subvec report i)))
   (range 1 (inc (count report)))))

(let [reports (parse-reports)]
  (count (filter safe? reports)))

(let [reports (parse-reports)]
  (count (filter (fn [report]
                   (some safe?
                         (cons report (dampened-reports (into [] report)))))
                 reports)))

