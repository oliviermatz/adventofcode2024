(ns advent2024.day07
  (:require [advent2024.common :refer [read-resource-lines all-combinations]]
            [clojure.string :refer [split trim join]]))

(defn parse-input []
  (map (fn [line]
         (let [[result operands-s] (split line #":")
               operands (split (trim operands-s) #" ")]
           {:result (parse-long result) :operands (map parse-long operands)}))
       (read-resource-lines "day07.txt")))

(defn evaluate [operands operators]
  (reduce (fn [result [operand operator]]
            (operator result operand))
          (first operands)
          (map vector (rest operands) operators)))

(defn get-total-calibration-results [ops equations]
  (apply +
         (map :result (filter (fn [{:keys [result operands]}]
                                (some (fn [operators]
                                        (= (evaluate operands operators) result))
                                      (all-combinations [] ops (dec (count operands)))))
                              equations))))

(get-total-calibration-results [* +] (parse-input))

(defn || [n1 n2]
  (parse-long (join [(.toString n1) (.toString n2)])))

(get-total-calibration-results [* + ||] (parse-input))