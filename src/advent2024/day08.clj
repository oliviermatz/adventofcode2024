(ns advent2024.day08
  (:require [advent2024.common :refer [read-resource-lines grid-at all-combinations point- point+ grid-in-bounds?]]))

(defn parse-input []
  (into [] (map #(into [] %) (read-resource-lines "day08.txt"))))

(defn get-antennas-grouped [grid]
  (->> (for [y (range 0 (count grid))
             x (range 0 (count (first grid)))]
         (let [p {:x x :y y}
               v (grid-at grid p)]
           (if (not= v \.)
             [[v p]]
             [])))
       (apply concat)
       (reduce (fn [acc [v p]]
                 (update acc v (fn [old] (if (nil? old) [p] (conj old p)))))
               {})))

(defn collect-all-antinodes [antennas-grouped get-antinodes-for]
  (->> antennas-grouped
       (mapcat (fn [[_ ps]]
                 (mapcat (partial apply get-antinodes-for)
                         (filter (partial apply not=)
                                 (all-combinations [] ps 2)))))
       (into #{})))

(let [grid (parse-input)]
  (->> (collect-all-antinodes
        (get-antennas-grouped grid)
        (fn [a b]
          (filter (partial grid-in-bounds? grid)
                  [(point+ a (point- a b))
                   (point+ b (point- b a))])))
       (count)))

(let [grid (parse-input)]
  (->> (collect-all-antinodes
        (get-antennas-grouped grid)
        (fn [a b]
          (let [ab (point- a b)
                ba (point- b a)
                in-bounds? (partial grid-in-bounds? grid)]
            (concat
             (take-while in-bounds?
                         (iterate (partial point+ ab) a))
             (take-while in-bounds?
                         (iterate (partial point+ ba) b))))))
       (count)))


