(ns advent2024.day06
  (:require [advent2024.common :refer [read-resource-lines grid-at point+]]))

(defn find-initial-position [grid]
  (some (fn [[y row]]
          (if-let [x (some (fn [[x cell]]
                             (if (= cell \^)
                               x
                               false))
                           (map-indexed vector row))]
            {:x x :y y}
            false))
        (map-indexed vector grid)))

(def dirs
  (cycle [{:x 0 :y -1}
          {:x 1 :y 0}
          {:x 0 :y 1}
          {:x -1 :y 0}]))

(defn walk [grid p dirs]
  (let [dir (first dirs)
        next-p (point+ p dir)
        next-v (grid-at grid next-p)]
    (cond
      (nil? next-v) (list {:p p :dir dir})
      (= next-v \#) (walk grid p (rest dirs))
      :else (lazy-seq (cons {:p p :dir dir}
                            (walk grid next-p dirs))))))

(defn has-cycle? [grid p]
  (loop [path (walk grid p dirs)
         seen #{}]
    (cond
      (empty? path) false
      (seen (first path)) true
      :else (recur (rest path) (conj seen (first path))))))

(let [grid (map #(into [] %) (read-resource-lines "day06.txt"))]
  (count (into #{} (map :p (walk grid (find-initial-position grid) dirs)))))

(let [grid (into [] (map #(into [] %) (read-resource-lines "day06.txt")))
      init-p (find-initial-position grid)
      candidates (disj (into #{} (map :p (walk grid init-p dirs)))
                       init-p)]
                       (count
                        (filter (fn [p]
                                  (let [new-grid (update grid (:y p) (fn [row] (assoc row (:x p) \#)))]
                                    (has-cycle? new-grid init-p)))
                                candidates)))