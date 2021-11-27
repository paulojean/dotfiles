#!/usr/bin/env bb

(ns i3-workspace-applications
  (:require [cheshire.core :as json]
            [clojure.string :as str]))

(defn search-in-tree
  "When provide `acc` accumulate result in it, otherwise return the first match"
  ([nodes pred] (search-in-tree nodes pred nil))
  ([[node & tail] pred acc]
   (cond
     (nil? node) acc

     (and (not (nil? acc)) (pred node)) (recur tail pred (conj acc node))

     (pred node) node

     :default (recur (concat (:nodes node) tail) pred acc))))

(defn nodes->applications [nodes]
  (search-in-tree nodes (comp not nil? :window_properties) []))

(defn get-workspace [number nodes]
  (search-in-tree nodes (comp (partial = number) :num)))

(defn workspace-applications [tree workspace]
  (->> (json/parse-string tree true)
       :nodes
       (get-workspace workspace)
       :nodes
       nodes->applications
       (map (comp :class :window_properties))
       set))

(when (= (System/getProperty "babashka.file")
         *file*)
  (->> (workspace-applications (first *command-line-args*)
                               (Integer/parseInt (second *command-line-args*)))
       (str/join ",")
       println))
