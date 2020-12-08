(ns tylerwardhaugh.advent-of-code-2020.day-08
  (:require [clojure.java.io :as io])
  (:require [clojure.edn :as edn])
  (:require [clojure.string :as str])
  (:require [net.cgrand.xforms.io :as xio]))

(defn parse-data
  "Parse our input data into a vector of instructions."
  []
  (let [source (xio/lines-in (io/resource "day-08.input.txt"))
        xf (map (comp (fn [[instr val]] (vector (keyword instr) (edn/read-string val)))
                      #(str/split % #" " 2)))]
    (into [] xf source)))

(defn task-1
  "Solve task 1"
  [instructions]
  (let [cnt (count instructions)]
    (loop [index 0
           acc 0
           seen #{}]
      (cond
        (>= index cnt) {:completes acc}
        (seen index) {:halts acc}
        :else (let [[instr val] (instructions index)
                    new-seen (conj seen index)]
                (case instr
                  :nop (recur (inc index) acc new-seen)
                  :acc (recur (inc index) (+ acc val) new-seen)
                  :jmp (recur (+ index val) acc new-seen)))))))

(defn task-2
  "Solve task 2"
  [instructions]
  (let [make-new (fn [index pair]
                   (vec (concat (subvec instructions 0 index)
                                (vector (replace {:jmp :nop, :nop :jmp} pair))
                                (subvec instructions (inc index)))))]
    (reduce
      (fn [index [instr _ :as pair]]
        (if (= instr :acc)
          (inc index)
          (let [new-instructions (make-new index pair)
                result (task-1 new-instructions)]
            (if-let [acc (:completes result)] (reduced acc) (inc index)))))
      0
      instructions)))

(defn -main
  "Solve both tasks"
  []
  (let [instructions (parse-data)
        t1 (-> instructions task-1 :halts)
        t2 (-> instructions task-2)]
    (println [t1 t2])))
