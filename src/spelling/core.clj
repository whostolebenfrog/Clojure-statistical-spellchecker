(ns spelling.core
  (:require [clojure.java.io :as io]))

(defn words
  []
  (map #(.toLowerCase %) (mapcat #(re-seq #"\w+" %) (line-seq (io/reader (io/file "big.txt"))))))

(defn train
  ([]
     (frequencies (words)))
  ([n]
     (frequencies (take n (words)))))
