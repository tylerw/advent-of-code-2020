(ns tylerwardhaugh.advent-of-code-2020.day-09
  (:require [clojure.java.io :as io])
  (:require [clojure.math.combinatorics :as combo])
  (:require [net.cgrand.xforms :as x])
  (:require [net.cgrand.xforms.io :as xio]))

(defn parse-data
  "Parse our input data into a vector."
  []
  (let [source (xio/lines-in (io/resource "day-09.input.txt"))
        xf  (map bigint)]
    (into [] xf source)))

(defn task-1
  "Solve task 1."
  ([coll] (task-1 coll 25))
  ([coll size]
   (let [xf (comp
              (x/window (inc size)
                        (fn
                          ([] (vector))
                          ([m] m)
                          ([m x] (conj m x)))
                        (fn [m _]
                          (subvec m 1)))
              (drop size)
              (keep (fn [v]
                      (let [prev (set (subvec v 0 size))
                            combos (combo/combinations prev 2)
                            cur (v size)]
                        (when-not (some (fn [[x y]] (= cur (+ x y))) combos)
                          cur)))))]
     (first (sequence xf coll)))))

(defn task-2
  "Solve task 2."
  [invalid coll]
  (let [cnt (count coll)
        sublists (for [i (range (dec cnt))
                       j (range (inc i) cnt)]
                   (subvec coll i (inc j)))
        minmax (juxt (partial apply min) (partial apply max))
        xf (comp
             (keep (fn [v] (when (= invalid (reduce + v))
                             (reduce + (minmax v)))))
             (take 1))]
    (first (sequence xf sublists))))

(defn -main
  "Solve both tasks"
  []
  (let [data (parse-data)
        t1 (task-1 data)
        t2 (task-2 t1 data)]
    (println [t1 t2])))
