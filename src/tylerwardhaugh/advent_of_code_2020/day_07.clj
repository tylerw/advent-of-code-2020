(ns tylerwardhaugh.advent-of-code-2020.day-07
  (:require [tylerwardhaugh.advent-of-code-2020.utils :as u])
  (:require [loom.graph :as lg])
  (:require [loom.derived :as ld])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [net.cgrand.xforms.io :as xio]))

(defn parse-data
  "Parse our input data into a vector of maps of the form {parent kids-map}."
  []
  (let [source (xio/lines-in (io/resource "day-07.input.txt"))
        keywordize #(-> (str/escape % {\space \-}) keyword)
        parse-kids (juxt #(-> % second keywordize)
                         #(-> % first u/parse-int))
        xf (comp
             (filter #(not (str/includes? % "no other")))
             (map (fn [s] (map next (re-seq #"(\d+)? ?(\w+ \w+) bag" s))))
             (map (fn [[[_ parent] & kids]]
                    (hash-map (keywordize parent)
                              (apply merge {} (map parse-kids kids))))))]
    (into [] xf source)))

(defn build-graph
  "Build a weighted digraph based on the given rules."
  [rules]
  (apply lg/weighted-digraph rules))

(defn task-1
  "Solve task 1"
  [wdg]
  (-> wdg
      lg/transpose
      (ld/subgraph-reachable-from :shiny-gold)
      lg/nodes
      rest
      count))

(defn count-nested-bags
  "Count the number of nested bags required"
  [wdg node]
  (let [source (lg/successors wdg node)
        xf (map (fn [n] (* (lg/weight wdg node n) (count-nested-bags wdg n))))]
    (transduce xf + 1 source)))

(defn task-2
  "Solve task 2"
  [wdg]
  (-> wdg (count-nested-bags :shiny-gold) dec))

(defn -main
  "Solve both tasks"
  []
  (let [rules (parse-data)
        wdg (build-graph rules)]
    (println ((juxt task-1 task-2) wdg))))
