(ns tylerwardhaugh.advent-of-code-2020.day-01
  (:require [clojure.java.io :as io])
  (:require [clojure.math.combinatorics :as combo])
  (:require [net.cgrand.xforms.io :as xio]))

(def target 2020)

(def nums
  (let [source (xio/lines-in (io/resource "day-01.input.txt"))
        xf (map #(Integer/parseInt %))]
        nums (into [] xf source)))

(defn day1
  "Find n numbers that equal target in nums and return their product"
  [n]
  (->> (combo/combinations nums n)
       (filter (fn [coll] (= target (apply + coll))))
       first
       (apply *)))

(def task-1 (day1 2))

(def task-2 (day1 3))

(vector task-1 task-2)
