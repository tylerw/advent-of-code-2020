(ns tylerwardhaugh.advent-of-code-2020.day-05
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io])
  (:require [net.cgrand.xforms.io :as xio]))

(defn parse-data
  "Parse our input data into a map."
  []
  (let [source (xio/lines-in (io/resource "day-05.input.txt"))
        xf (map (comp #(Long/parseLong % 2)
                      #(str/escape % {\F 0, \B, 1 \L 0, \R 1})))]
    (into (sorted-set-by >) xf source)))

(def ^{:doc "Solve task 1"} task-1 first)

(defn task-2
  "Solve task 2"
  [coll]
  (->> coll
       (partition-all 2 1)
       (keep #(when (= 2 (apply - %)) (-> % first dec)))
       first))

(defn -main
  "Solve both tasks"
  []
  (let [coll (parse-data)]
    (println ((juxt task-1 task-2) coll))))
