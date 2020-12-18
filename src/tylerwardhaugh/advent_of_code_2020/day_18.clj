(ns tylerwardhaugh.advent-of-code-2020.day-18
  (:require [clojure.edn :as edn]
            [clojure.walk :as walk]
            [tylerwardhaugh.advent-of-code-2020.utils :as u]))

(defn process-input
  "Parse the input"
  []
  (let [xf (map (fn [line] (edn/read-string (str "(" line ")"))))]
    (into [] xf (u/day-input-source 18))))

(defn calc-eq
  "Calculate the value of the equation in Task 1's math system"
  [eq]
  (let [fns {'* *, '+ +}
        eval-terms (completing (fn [acc [op arg]] ((fns op) acc arg)))]
    (walk/postwalk
      (fn [term]
        (if (sequential? term)
          (transduce (partition-all 2) eval-terms (first term) (rest term))
          term))
      eq)))

(defn decorate-terms
  "Add parens around everything except multiplication."
  [eq]
  (let [xf (comp (partition-by #{'*})
                 (map #(if (= '(*) %) '* %)))]
    (walk/postwalk
      (fn [term]
        (if (sequential? term)
          (sequence xf term)
          term))
      eq)))

(defn task-1
  "Solve task 1."
  [input]
  (transduce (map calc-eq) + input))

(defn task-2
  "Solve task 2."
  [input]
  (let [xf (comp (map decorate-terms)
                 (map calc-eq))]
    (transduce xf + input)))

(defn -main
  "Solve both tasks."
  []
  (let [input (process-input)]
    (println ((juxt task-1 task-2) input))))
