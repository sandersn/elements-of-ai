(ns chapter3.match
  (:refer clojure.test :only [with-test is]))
(with-test
  (defn atom? [x] 
    (not (or (list? x) (nil? x) (instance? clojure.lang.Cons x))))
  (is (atom? 12))
  (is (not (atom? '())))
  (is (not (atom? nil)))
  (is (not (atom? '(1))))
  (is (not (atom? (cons 1 '(2 3)))))
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
(defn star-pattern-match-test [match f]
  (is (= {'x '(the whole thing)} (match '((* x)) '(the whole thing))))
  (is (= {'x '(whole thing) 'y 'the} (match '((? y) (* x)) '(the whole thing))))
  (is (not (match '(this (* x)) '(the whole thing))))
  (is (= {'x '(thing) 'y 'whole} (match '(the (? y) (* x)) '(the whole thing))))
  (is (= {'x '()} (match '(the whole thing (* x)) '(the whole thing))))
  (is (not (match (list 'the (list f 'y) 'thing) '(the whole thing)))))
(defn =1?-newentry [d x] 
  (and (= x 1) (assoc d 'z 'x-is-1)))
(defn =1?-update [d x] 
  (and (= x 2) (assoc d 'z (+ 1 (d 'x)))))
(defn =1?-update1 [d x] 
  (and (= x 2) (assoc d 'a (+ 1 (d 'x)))))
(defn =1?-update2 [d x] 
  (and (= x 3) (assoc d 'b (+ (d 'a) (d 'y)))))
(defn =1?-nostar [d x] 
  (and (= x 3) (assoc d 'a (d 'x))))
(defn state-pattern-match-test [match]
  (is (= {'x 1 'y 1 'z 'x-is-1} (match '((? x) (chapter3.match/=1?-newentry y) 3) '(1 1 3))))
  (is (not (match '((? x) (chapter3.match/=1?-newentry y) 3) '(1 2 3))))
  (is (= {'x 1 'y 2 'z 2} (match '((? x) (chapter3.match/=1?-update y) 3) '(1 2 3))))
  (is (= {'x 1 'y 2 'z 3 'a 2 'b 4} (match '((? x) (chapter3.match/=1?-update1 y) (chapter3.match/=1?-update2 z)) '(1 2 3))))
  ; * isn't captured until after resolve matches have a chance to look at the state
  (is (= {'x '(1) 'z 3 'a nil} (match '((* x) 2 (chapter3.match/=1?-nostar z)) '(1 2 3))))
)

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
  (is (not (match '(1 (2) 3) '(1 (2) 3))) "malformed pattern")
  (function-pattern-match-test match)
  (star-pattern-match-test match 'chapter3.match/=1?))
(with-test
  (defn match-state
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
        (do
          (swap! variables (fn [vs] (assoc vs (second (first p)) (first s))))
          (match-helper (rest p) (rest s)))
        (and (= (count (first p)) 2)
             (= (first (first p)) '*))
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
        (and (not (empty? s))
             (= (count (first p)) 2)
             (resolve (first (first p))))
        (when-let [update-d ((resolve (first (first p))) @variables (first s))]
          (swap! variables (fn [vs] (assoc (merge vs update-d) (second (first p)) (first s))))         
          (match-helper (rest p) (rest s)))
        :else false))
    (if (match-helper p s) @variables false))
  (basic-match-test match-state)
  (basic-pattern-match-test match-state)
  (is (not (match-state '(1 (2) 3) '(1 (2) 3))) "malformed pattern")
  (is (= {'x 1 'y 2 'z 'x-is-1} (match-state '((chapter3.match/=1?-newentry x) (? y) 3) '(1 2 3))))
  (is (not (match-state '((chapter3.match/=1?-newentry x) 2 3) '(2 2 3))))
  (star-pattern-match-test match-state 'chapter3.match/=1?-newentry)
  (state-pattern-match-test match-state)
  )
