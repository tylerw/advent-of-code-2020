(ns tylerwardhaugh.advent-of-code-2020.day-02
  (:require [tylerwardhaugh.advent-of-code-2020.utils :as u])
  (:require [clojure.java.io :as io])
  (:require [net.cgrand.xforms.io :as xio]))

(defn parse
  "Parse a line of input"
  [s]
  (let [[_ low high char password]
        (re-matches #"^(\d+)-(\d+)\s+([a-z]):\s+([a-z]+)$" s)]
    (zipmap [:low :high :char :password]
            [(u/parse-int low) (u/parse-int high) (first char) password])))

(defn parse-rules
  "Parse all lines of input into rules"
  []
  (let [source (xio/lines-in (io/resource "day-02.input.txt"))]
    (eduction (map parse) source)))

(defn task-1
  "Solve the first task"
  [rules]
  (let [xf (keep
             (fn [{:keys [low high char password]}]
               (let [cnt (-> (group-by #{char} password) (get char) count)]
                 (when (<= low cnt high) 1))))]
    (transduce xf + rules)))

(defn task-2
  "Solve the second task"
  [rules]
  (let [xf (keep
             (fn [{:keys [low high char password]}]
               (let [chars (map (vec password) [(dec low) (dec high)])]
                 (when (= (set (map #{char} chars)) #{nil char}) 1))))]
    (transduce xf + rules)))

(defn -main
  []
  (let [rules (parse-rules)]
    ((juxt task-1 task-2) rules)))
