(ns chapter3.match)
(def atom? (comp not list?))
(defn match2 [p s]
  (cond
    (atom? p) (atom? s)
    (atom? s) false
    (empty? p) (empty? s)
    (match2 (first p) (first s)) (match2 (rest p) (rest s))
    :else false))
