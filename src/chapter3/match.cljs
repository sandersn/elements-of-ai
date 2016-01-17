(ns chapter3.match)
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
(defn match5
  "Note: ClojureScript doesn't seem to have resolve (or eval) so this code
  won't actually work there."
  [p s]
  (def variables (atom {}))
  (defn match-helper [p s]
    (cond
      (or (atom? p) (atom? s)) false
      (empty? p) (empty? s)
      (= (first p) (first s)) (match5 (rest p) (rest s))
      (and
       (= (count (first p)) 2)
       (= (first (first p)) '?)
       (match5 (rest p) (rest s)))
      (do
        (swap! variables (fn [vs] (assoc vs (first (rest (first p))) (first s))))
        true)
      (and
       (= (count (first p)) 2)
       (not (= (first (first p)) '?))
       ((resolve (first (first p))) (first s))
       (match5 (rest p) (rest s)))
      (do
        (swap! variables (fn [vs] (assoc vs (first (rest (first p))) (first s))))
        true)
    :else false))
  (if (match-helper p s) @variables false))
(defn match
  "Note: ClojureScript doesn't have resolve so arbitrary predicates won't work there."
  [p s]
  (def variables (atom {}))
  (defn match-helper [p s]
    (cond
      ; (or (atom? p) (atom? s)) false
      (empty? p) (empty? s)
      (atom? (first p))
      (and (not (empty? s))
           (= (first p) (first s))
           (match (rest p) (rest s)))
      ; complex patterns
      (and (not (empty? s))
           (= (first (first p)) '?))
      (when (match (rest p) (rest s))
        (swap! variables (fn [vs] (assoc vs (first (rest (first p))) (first s))))
        true)
      (= (first (first p)) '*)
      (cond
        (and (not (empty? s))
             (match (rest p) (rest s)))
        (do
          (swap! variables (fn [vs] (assoc vs (first (rest (first p))) (list (first s)))))
          true)
        (match (rest p) s)
        (do
          (swap! variables (fn [vs] (assoc vs (first (rest (first p))) (list))))
          true)
        (and (not (empty? s))
             (match p (rest s)))
        (do
          (swap! variables (fn [vs] (assoc vs
                                           (first (rest (first p))) 
                                           (cons (first s) (vs (first (rest (first p))))))))
          true))
      (and
       (not (empty? s))
       ((resolve (first (first p))) (first s))
       (match (rest p) (rest s)))
      (do
        (swap! variables (fn [vs] (assoc vs (first (rest (first p))) (first s))))
        true)
    :else false))
  (if (match-helper p s) @variables false))
