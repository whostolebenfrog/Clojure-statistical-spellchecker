(ns spelling.core
  (:require [clojure.java.io :as io]))

(defn words
  "Returns a lazy sequence of words from out input set"
  []
  (map #(.toLowerCase %)
       (mapcat #(re-seq #"\w+" %) (line-seq (io/reader (io/file "big.txt"))))))

(def nwords (delay (frequencies (words))))

(defn distance-1
  "The set of words that are one difference away from our example word"
  [word]
  (let [alphabet (seq "abcdefghijklmnopqrstuvwxyz")
        len      (count word)

        dels     (for [i (range len)]
                   (str (subs word 0 i) (subs word (inc i))))

        trans    (for [i (range (dec len))]
                   (str (subs word 0 i) (nth word (inc i))
                        (nth word i) (subs word (+ 2 i))))

        replaces (for [i (range len) l alphabet]
                   (str (subs word 0 i) l (subs word (inc i))))

        inserts  (for [i (range 1 (inc len)) l alphabet]
                   (str (subs word 0 i) l (subs word i)))]

    (distinct (concat dels trans replaces inserts))))

(defn distance-2
  [word]
  "The set known of words that are two differences away from our example word"
  (distinct (for [w1 (distance-1 word) w2 (distance-1 w1)]
              w2)))

(defn known
  [words]
  "Return the union of the input set and supplied sets of words or nil if empty"
  (not-empty (set (filter @nwords words))))

(defn correct
  [word]
  "Returns the word if in input set, or our correction"
  (let [candidates (or (known [word])
                       (known (distance-1 word))
                       (known (distance-2 word))
                       [word])]
    (apply max-key #(@nwords %) candidates)))
