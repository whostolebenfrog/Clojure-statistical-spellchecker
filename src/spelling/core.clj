(ns spelling.core
  (:require [clojure.java.io :as io]))

(defn words
  "Returns a lazy sequence of words from out input set"
  []
  (map #(.toLowerCase %) (mapcat #(re-seq #"\w+" %) (line-seq (io/reader (io/file "big.txt"))))))

(defn train
  "Returns a map of words from the input set to their frequencies."
  ([]
     (frequencies (words)))
  ([n]
     (frequencies (take n (words)))))

(def nwords (train))

(defn second-has
  "Second part of the seq has greater than n elements"
  [seq n]
  (filter #(not-empty (nthrest (second %) n)) seq))

(defn transposed
  "Returns f concatenated to s with its first two letters switched"
  [f s]
  (concat f [(second s) (first s)] (nthrest s 2)))

(defn edits-distance-1
  "The set of words that are one mistake away from our example word"
  [word]
  (let [alphabet   (seq "abcdefghijklmnopqrstuvwxyz")
        splits     (map #(split-at % word) (range (inc (count word))))
        deletes    (map #(concat (first %) (rest (second %))) (second-has splits 0))
        transposes (map #(transposed (first %) (second %)) (second-has splits 1))
        replaces   (apply concat (for [letter alphabet]
                                   (map #(concat (first %) [letter] (rest (second %)))
                                        (second-has splits 0))))
        inserts    (apply concat (for [letter alphabet]
                                   (map #(concat (first %) [letter] (second %))
                                        splits)))]

    (set (concat deletes transposes replaces inserts))))

(defn known
  "Returns the union of words and our input set"
  [words]
  (filter nwords words))

(defn known-edits-distance-2
  [word]
  "The set of words that are two mistakes away from our example word"
  (filter nwords (apply concat (for [w1 (edits-distance-1 word)]
                                 (for [w2 (edits-distance-1 w1)]
                                   w2)))))

(filter #{"the"} (edits-distance-1 "th"))
