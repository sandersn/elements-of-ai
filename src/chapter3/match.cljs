(ns chapter3.match
  (:require [cljs.nodejs :as nodejs]))
(nodejs/enable-util-print!)
(def atom? (comp not list?))
(defn match2 [p s]
  (cond
    (atom? p) (atom? s)
    (atom? s) false
    (empty? p) (empty? s)
    (match2 (first p) (first s)) (match2 (rest p) (rest s))
    :else false))
(defn match3 [p s]
  (cond
    (or (atom? p) (atom? s)) false
    (empty? p) (empty? s)
    (= (first p) (first s)) (match3 (rest p) (rest s))
    (= (first p) '?) (match3 (rest p) (rest s))
    :else false))
(defn match4 [p s]
  (def variables (atom {}))
  (defn match-helper [p s]
    (cond
      (or (atom? p) (atom? s)) false
      (empty? p) (empty? s)
      (= (first p) (first s)) (match4 (rest p) (rest s))
      (and
       (= (count (first p)) 2)
       (= (first (first p)) '?)
       (match4 (rest p) (rest s)))
      (do
        (swap! variables (fn [vs] (assoc vs (first (rest (first p))) (first s))))
        true)
    :else false))
  (if (match-helper p s) @variables false))
