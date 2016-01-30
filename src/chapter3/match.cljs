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
        (swap! variables (fn [vs] (assoc vs (second (first p)) (first s))))
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
        (swap! variables (fn [vs] (assoc vs (second (first p)) (first s))))
        true)
      (and
       (= (count (first p)) 2)
       (not (= (first (first p)) '?))
       ((resolve (first (first p))) (first s))
       (match5 (rest p) (rest s)))
      (do
        (swap! variables (fn [vs] (assoc vs (second (first p)) (first s))))
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
           (match-helper (rest p) (rest s)))
      ; complex patterns
      (and (not (empty? s))
           (= (first (first p)) '?))
      (when (match-helper (rest p) (rest s))
        (swap! variables (fn [vs] (assoc vs (second (first p)) (first s))))
        true)
      (= (first (first p)) '*)
      (cond
        (and (not (empty? s))
             (match-helper (rest p) (rest s)))
        (do
          (swap! variables (fn [vs] (assoc vs (second (first p)) (list (first s)))))
          true)
        (match-helper (rest p) s)
        (do
          (swap! variables (fn [vs] (assoc vs (second (first p)) (list))))
          true)
        (and (not (empty? s))
             (match-helper p (rest s)))
        (do
          (swap! variables (fn [vs] (assoc vs
                                           (second (first p)) 
                                           (cons (first s) (vs (second (first p)))))))
          true))
      (and
       (not (empty? s))
       ((resolve (first (first p))) (first s))
       (match-helper (rest p) (rest s)))
      (do
        (swap! variables (fn [vs] (assoc vs (second (first p)) (first s))))
        true)
      :else false))
  (if (match-helper p s) @variables false))
(defn match-state
  "Note: ClojureScript doesn't have resolve so arbitrary predicates won't work there."
  [p s]
  (def variables (atom {}))
  (defn match-helper [p s]
    (cond
      (empty? p) (empty? s)
      (atom? (first p))
      (and (not (empty? s))
           (= (first p) (first s))
           (match-helper (rest p) (rest s)))
      ; complex patterns
      (and (not (empty? s))
           (= (first (first p)) '?))
      (when (match-helper (rest p) (rest s))
        (swap! variables (fn [vs] (assoc vs (second (first p)) (first s))))
        true)
      (= (first (first p)) '*)
      (cond
        (and (not (empty? s))
             (match-helper (rest p) (rest s)))
        (do
          (swap! variables (fn [vs] (assoc vs (second (first p)) (list (first s)))))
          true)
        (match-helper (rest p) s)
        (do
          (swap! variables (fn [vs] (assoc vs (second (first p)) (list))))
          true)
        (and (not (empty? s))
             (match-helper p (rest s)))
        (do
          (swap! variables (fn [vs] (assoc vs
                                           (second (first p)) 
                                           (cons (first s) (vs (second (first p)))))))
          true))
      (not (empty? s))
      (when-let [update-d ((resolve (first (first p))) @variables (first s))]
        (when (match-helper (rest p) (rest s))
          (swap! variables (fn [vs] (assoc (merge vs update-d) (second (first p)) (first s))))
          true))
      :else false))
  (if (match-helper p s) @variables false))
