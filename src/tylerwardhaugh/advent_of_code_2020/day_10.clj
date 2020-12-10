(ns tylerwardhaugh.advent-of-code-2020.day-10
  (:require [tylerwardhaugh.advent-of-code-2020.utils :as u]
            [net.cgrand.xforms :as x]))

(defn process-input
  "Parse the input into a sequence of integers, adding 0 and (max)+3,
  then sorting it and returning it as a vector."
  []
  (let [initial (into [0] (map u/parse-int) (u/day-input-source 10))]
    (-> (conj initial (+ 3 (reduce max initial)))
        sort
        vec)))

(defn task-1
  "Solve task 1."
  [input]
  (let [xf (comp (x/partition 2 1) (map #(apply - %)) (x/by-key - x/count))
        freqs (into {} xf input)]
    (transduce (map freqs) * [1 3])))

; info on memoizing recursive functions:
;   https://stackoverflow.com/questions/27445876/is-there-a-simpler-way-to-memoize-a-recursive-let-fn
(defn task-2
  "Solve task 2."
  [input]
  (let [next-jolts
        (fn [j] (sequence (mapcat (comp #(filter #{%} input) #(apply + %)))
                          (map vector [1 2 3] (repeat 3 j))))
        sum-arrangements
        (memoize (fn [self cur-jolt]
                   (if-let [jolts (seq (next-jolts cur-jolt))]
                     (let [xf (map (partial self self))]
                       (transduce xf + (dec (count jolts)) jolts))
                     0)))
        sum-arrangements (partial sum-arrangements sum-arrangements)]
    (inc (sum-arrangements 0))))

(defn -main
  "Solve both tasks."
  []
  (let [input (process-input)]
    (println ((juxt task-1 task-2) input))))
