(ns chapter3.match
  (:refer clojure.test :only [with-test is deftest]))
(with-test
  (defn atom? [x] 
    (not (or (list? x) (nil? x))))
  (is (atom? 12))
  (is (not (atom? '())))
  (is (not (atom? nil)))
  (is (not (atom? '(1))))
  (is (atom? [])) ; this is basically wrong -- don't use vectors with chapter3.match
  (is (atom? {}))
  (is (atom? true)))

(defn basic-match-test [match]
  (is (not (match 12 '(12)))) 
  (is (not (match '(12) 12)))
  (is (match '() '()))
  (is (not (match '() '(1))))
  (is (not (match '(1) '())))) 
(defn recursive-match-test [match]
  (is (match '(1 (2) 3) '(1 (2) 3))) 
  (is (not (match '(1 (2 nope) 3) '(1 (2 not really) 3)))))
(defn basic-pattern-match-test [match]
  (is (match '(1 (? x) 2) '(1 not-really 2)))
  (is (= 'not-really ('x (match '(1 (? x) 2) '(1 not-really 2)))))
  (is (= nil ('y (match '(1 (? x) 2) '(1 caught 2)))))
  (is (= {} (match '(1 2 3) '(1 2 3))))
  (is (= {'x 1 'y 2} (match '((? x) (? y) 3) '(1 2 3))))
  (is (not (match '(1 3) '(1 2))))
  (is (not (match '(1 (?) 3) '(1 (2) 3))))
  (is (not (match '(1 (? x y) 3) '(1 (2) 3))))
  (is (not (match '(1 (! x) 3) '(1 (2) 3))))
  (is (not (match '((? x) 3) '(1 2)))))
(defn =1? [x] (= x 1))
(defn function-pattern-match-test [match]
  (is (= {'x 1 'y 2} (match '((chapter3.match/=1? x) (? y) 3) '(1 2 3))))
  (is (not (match '((chapter3.match/=1? x) 2 3) '(2 2 3)))))

(with-test
  (defn match2 [p s]
    (cond
      (atom? p) (atom? s)
      (atom? s) false
      (empty? p) (empty? s)
      (match2 (first p) (first s)) (match2 (rest p) (rest s))
      :else false))
  (is (match2 12 12)) ; unlike the rest, match2 matches bare atoms
  (basic-match-test match2)
  (recursive-match-test match2))
(with-test
  (defn match3 [p s]
    (cond
      (or (atom? p) (atom? s)) false
      (empty? p) (empty? s)
      (= (first p) (first s)) (match3 (rest p) (rest s))
      (= (first p) '?) (match3 (rest p) (rest s))
      :else false))
  (basic-match-test match3)
  (recursive-match-test match3))
(with-test
  (defn match4 [p s]
    (def variables (atom {}))
    (defn match-helper [p s]
      (cond
        (or (atom? p) (atom? s)) false
        (empty? p) (empty? s)
        (= (first p) (first s)) (match4 (rest p) (rest s))
        (and
         (not (atom? (first p)))
         (= (count (first p)) 2)
         (= (first (first p)) '?)
         (match4 (rest p) (rest s)))
        (do
          (swap! variables (fn [vs] (assoc vs (second (first p)) (first s))))
          true)
        :else false))
    (if (match-helper p s) @variables false))
  (basic-match-test match4)
  (basic-pattern-match-test match4)
  ; match4 and match5 allow malformed patterns as literals
  (is (= {} (match4 '(1 (2) 3) '(1 (2) 3)))))
(with-test
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
         (not (atom? (first p)))
         (= (count (first p)) 2)
         (= (first (first p)) '?)
         (match5 (rest p) (rest s)))
        (do
          (swap! variables (fn [vs] (assoc vs (second (first p)) (first s))))
          true)
        (and
         (not (atom? (first p)))
         (= (count (first p)) 2)
         (not (= (first (first p)) '?))
         (resolve (first (first p)))
         ((resolve (first (first p))) (first s))
         (match5 (rest p) (rest s)))
        (do
          (swap! variables (fn [vs] (assoc vs (second (first p)) (first s))))
          true)
        :else false))
    (if (match-helper p s) @variables false))
  (basic-match-test match5)
  (basic-pattern-match-test match5)
  (is (= {} (match5 '(1 (2) 3) '(1 (2) 3))))
  (function-pattern-match-test match5))
(with-test
  (defn match
  "Note: ClojureScript doesn't have resolve so arbitrary predicates won't work there."
    [p s]
    (def variables (atom {}))
    (defn match-helper [p s]
      (cond
        (or (atom? p) (atom? s)) false
        (empty? p) (empty? s)
        (atom? (first p))
        (and (not (empty? s))
             (= (first p) (first s))
             (match-helper (rest p) (rest s)))
        ; complex patterns
        (and (not (empty? s))
             (= (count (first p)) 2)
             (= (first (first p)) '?))
        (when (match-helper (rest p) (rest s))
          (swap! variables (fn [vs] (assoc vs (second (first p)) (first s))))
          true)
        (and (= (first (first p)) '*)
             (= (count (first p)) 2))
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
         (= (count (first p)) 2)
         (resolve (first (first p)))
         ((resolve (first (first p))) (first s))
         (match-helper (rest p) (rest s)))
        (do
          (swap! variables (fn [vs] (assoc vs (second (first p)) (first s))))
          true)
        :else false))
    (if (match-helper p s) @variables false))
  (basic-match-test match)
  (basic-pattern-match-test match)
  (is (not (match '(1 (2) 3) '(1 (2) 3))))
  (function-pattern-match-test match))
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
