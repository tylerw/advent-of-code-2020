(ns tylerwardhaugh.advent-of-code-2020.day-03
  (:require [clojure.java.io :as io])
  (:require [net.cgrand.xforms.io :as xio])
  (:require [clojure.core.matrix :as m]))

(defn parse-map
  "Parse our input data into a map."
  []
  (let [source (xio/lines-in (io/resource "day-03.input.txt"))
        xf (map (comp (partial replace {\. 0, \# 1}) seq))]
    (m/matrix (into [] xf source))))

(defn count-trees
  "Count the trees in a slope"
  [treemap right down]
  (let [[rows cols] (m/shape treemap)
        indices (mapv vector
                      (range 0 rows down)
                      (iterate (fn [j] (mod (+ right j) cols)) 0))]
    (reduce + (m/select-indices treemap indices))))

(let [treemap (parse-map)]
  (defn task-1
    "Solve task 1"
    []
    (count-trees treemap 3 1))

  (defn task-2
    "Solve task 2"
    []
    (let [slopes [[1 1] [3 1] [5 1] [7 1] [1 2]]]
      (transduce (map (fn [[r d]] (count-trees treemap r d))) * slopes))))
