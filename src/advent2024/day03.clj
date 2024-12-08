(ns advent2024.day03
  (:require [advent2024.common :refer [read-resource-lines]]))

(let [ops (->> (read-resource-lines "day03.txt")
               (map #(re-seq #"mul\((\d+),(\d+)\)" %))
               (apply concat))]
  (reduce (fn [acc [_ l r]]
            (+ acc (* (parse-long l) (parse-long r))))
          0
          ops))

(let [ops (->> (read-resource-lines "day03.txt")
               (map #(re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)" %))
               (apply concat))]
  (reduce (fn [{:keys [active sum]} [op l r]]
            (case op
              "do()" {:active true :sum sum}
              "don't()" {:active false :sum sum}
              (if active
                {:active true :sum (+ sum (* (parse-long l) (parse-long r)))}
                {:active false :sum sum})))
          {:active true :sum 0}
          ops))

