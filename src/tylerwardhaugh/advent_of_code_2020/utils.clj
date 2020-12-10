(ns tylerwardhaugh.advent-of-code-2020.utils
  (:require [clojure.java.io :as io]
            [net.cgrand.xforms.io :as xio]))

; files
(defn day-input-resource
  "A day's input file."
  [n]
  (->> n (format "day-%02d.input.txt") io/resource))

(defn day-input-source
  "A reducible view of a day's input (suitable for a transducer source).

  Note: one must use a IReduceInit-aware method to extract values; sequence,
  for example, will not work. But {re,trans}duce, into, eduction, etc. will."
  [n]
  (-> n day-input-resource xio/lines-in))

; numbers
(defn parse-int
  "Parse the string `d` as an integer"
  [d]
  (Integer/parseInt d))

(defn parse-long
  "Parse the string `d` as a long"
  [d]
  (Long/parseLong d))
