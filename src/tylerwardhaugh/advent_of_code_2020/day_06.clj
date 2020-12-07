(ns tylerwardhaugh.advent-of-code-2020.day-06
  (:require [clojure.set :as set])
  (:require [clojure.java.io :as io])
  (:require [net.cgrand.xforms.io :as xio]))

(defn parse-data
  "Parse our input data into a map."
  []
  (let [source (xio/lines-in (io/resource "day-06.input.txt"))
        xf (partition-by empty?)]
    (eduction xf source)))

(defn task-1
  "Solve task 1"
  [coll]
  (let [xf (map (comp count
                      (partial reduce into #{})))]
    (transduce xf + coll)))

(defn task-2
  "Solve task 2"
  [coll]
  (let [xf (map (comp count
                      (partial apply set/intersection)
                      (partial map set)))]
    (transduce xf + coll)))

(defn -main
  "Solve both tasks"
  []
  (let [coll (parse-data)]
    (println ((juxt task-1 task-2) coll))))
; => [6521 3305]
