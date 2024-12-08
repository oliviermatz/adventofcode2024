(ns advent2024.common
  (:require [clojure.java.io :refer [resource reader]]))

(defn extract-regex-groups [regex s]
  (->> s
       (re-matcher regex)
       (re-find)
       (rest)))

(defn read-resource-lines [res-name]
  (->> res-name
       resource
       reader
       line-seq))

(defn read-resource-lines-extract [res-name regex]
  (->> (read-resource-lines res-name)
       (map (partial extract-regex-groups regex))))

(defn grid-in-bounds? [grid {:keys [x y]}]
  (let [y-max (count grid)
        x-max (count (first grid))]
    (and (>= x 0) (< x x-max)
         (>= y 0) (< y y-max))))

(defn grid-at [grid p]
  (if (grid-in-bounds? grid p)
    (nth (nth grid (:y p)) (:x p))
    nil))

(defn point+ [p1 p2]
  {:x (+ (:x p1) (:x p2))
   :y (+ (:y p1) (:y p2))})

(defn point- [p1 p2]
  {:x (- (:x p1) (:x p2))
   :y (- (:y p1) (:y p2))})

(defn seq-in-direction [grid p dir]
  (let [v (grid-at grid p)]
    (if (nil? v)
      '()
      (lazy-seq (cons v (seq-in-direction grid (point+ p dir) dir))))))

(defn all-combinations [prefix chars len]
  (if (= len 0)
    [prefix]
    (apply concat
           (for [c chars]
             (all-combinations (conj prefix c) chars (dec len))))))
