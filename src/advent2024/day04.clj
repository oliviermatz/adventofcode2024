(ns advent2024.day04
  (:require [advent2024.common :refer [read-resource-lines point+ seq-in-direction]]))

(def all-dirs
  [{:x 0 :y 1}
   {:x 1 :y 1}
   {:x 1 :y 0}
   {:x 1 :y -1}
   {:x 0 :y -1}
   {:x -1 :y -1}
   {:x -1 :y 0}
   {:x -1 :y 1}])

(defn collect-around-at [xs grid p]
  (for [dir all-dirs
        :let [cur (take (count xs) (seq-in-direction grid p dir))]
        :when (= cur xs)]
    {:p p :dir dir :content cur}))

(let [grid (->> (read-resource-lines "day04.txt")
                (map #(into [] %))
                (into []))]
  (count
   (apply
    concat
    (for [y (range 0 (count grid))
          x (range 0 (count (first grid)))]
      (collect-around-at [\X \M \A \S] grid {:x x :y y})))))

(let [grid (->> (read-resource-lines "day04.txt")
                (map #(into [] %))
                (into []))]

  (count
   (for [y (range 0 (count grid))
         x (range 0 (count (first grid)))
         :let [p {:x x :y y}
               top-left (take 3
                              (seq-in-direction grid
                                          (point+ p {:x -1 :y -1})
                                          {:x 1 :y 1}))
               top-right (take 3
                               (seq-in-direction grid
                                           (point+ p {:x 1 :y -1})
                                           {:x -1 :y 1}))]
         :when (and (or (= top-left [\M \A \S]) (= top-left [\S \A \M]))
                    (or (= top-right [\M \A \S]) (= top-right [\S \A \M])))]
     {:x x :y y})))

