(ns tylerwardhaugh.advent-of-code-2020.day-04
  (:require [tylerwardhaugh.advent-of-code-2020.utils :as u])
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io])
  (:require [clojure.spec.alpha :as s])
  (:require [net.cgrand.xforms.io :as xio]))

(defn parse-data
  "Parse our input data into a map."
  []
  (let [source (xio/lines-in (io/resource "day-04.input.txt"))
        xf (comp
             ; split into records
             (partition-by (partial = ""))
             (filter (partial not= [""]))
             ; split each record into seq of key/value pairs
             (map (comp
                    (fn [s] (->> (re-seq #"\s*([a-z]{3}):(\S+)\s*" s)
                                 (map (partial drop 1))))
                    (partial str/join " ")))
             ; put each record into a map
             (map (comp (partial into {})
                        (partial map (fn [[k v]] [(keyword k) v])))))]
    (into [] xf source)))

; passport specs
(s/def ::byr #(<= 1920 (u/parse-int %) 2002))
(s/def ::iyr #(<= 2010 (u/parse-int %) 2020))
(s/def ::eyr #(<= 2020 (u/parse-int %) 2030))
(s/def ::hgt (fn [s]
               (when-let [[_ figure unit] (re-find #"^(\d+)(cm|in)$" s)]
                (cond
                  (= unit "cm") (<= 150 (u/parse-int figure) 193)
                  (= unit "in") (<= 59 (u/parse-int figure) 76)
                  :else false))))
(s/def ::hcl #(re-matches #"\#[0-9a-f]{6}" %1))
(s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def ::pid #(re-matches #"[0-9]{9}" %1))

(s/def ::task-1 #(= (-> % (dissoc :cid) keys set)
                    #{:byr :ecl :eyr :hcl :hgt :iyr :pid}))
(s/def ::task-2
  (s/and ::task-1
         (s/keys :req-un [::byr ::ecl ::eyr ::hcl ::hgt ::iyr ::pid]
                 :opt-un [::cid])))

(defn count-valids
    "Count the number of valid passports"
    [passports spec]
    (transduce (keep #(when (s/valid? spec %) 1)) + passports))

(defn -main
  "Solve both tasks"
  []
  (let [passports (parse-data)]
    (println (map (partial count-valids passports) [::task-1 ::task-2]))))
