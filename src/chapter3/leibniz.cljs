(ns chapter3.leibniz
  (:refer chapter3.match :only [match match-state atom?]))
(defn nested-diff-sum? [d v]
  (and (not (atom? v)) (match '(+ (? e1) (? e2)) v)))
(defn nested-diff-x1? [d v]
  {:e1 v})
(defn nested-diff-x2? [d v]
  (= v (d :e1)))
(def diff-sum-rule
  ['differentiate
   '(d (nested-diff-sum? e3) (? v1))
   #(list '+ (list 'd (% :e1) (% :v1)) (list 'd (% :e2) (% :v1)))
   "diff-sum-rule"])
(def diff-x-rule
  ['differentiate
   '(d (nested-diff-x1? e1) (nested-diff-x2? e2))
   1
   "diff-x-rule"])
; other rules go here ...

; TODO: Clojure HAS to have a class/record facility to do this...
(def rule-goal first)
(def rule-pattern second)
(defn rule-action [rule] (nth rule 2))
(defn rule-name [rule] (nth rule 3))
(def rules
  [diff-sum-rule diff-x-rule])
;;; control scheme ;;;
; (I don't know--that's just what the book calls it)
(declare try-rule1)
(defn try-rule-on-list [rule expressions]
  (if (empty? expressions)
    nil
    (if-let [temp (try-rule1 rule (first expressions))]
      (cons temp (rest expressions))
      (when-let [temp (try-rule-on-list rule (rest expressions))]
        (cons (first expressions) temp)))))
(defn fire [rule]
  (println (rule-name rule) " fires.")
  ((rule-action rule)))
(defn try-rule1 [rule expression]
  (cond
    (atom? expression) nil
    (match-state (rule-pattern rule) expression) (fire rule)
    :else (try-rule-on-list rule expression)))
(defn try-rule [rule expression]
  (if (= current-goal (rule-goal rule))
    (try-rule1 rule expression)))
(defn try-rules [rules formula]
  (loop [rules-left rules]
    (cond
      (empty? rules-left) nil ; no rules applied -- fail 
      (atom? formula) nil ; ??? (reduced to an atom?) -- fail
      :else (if-let [new-formula (try-rule (first rules-left) formula)]
              new-formula
              (recur (rest rules-left) formula)))))
(defn control []
  ; TODO: current-formula is unbound
  (loop [current-formula '()]
      (if-let [new-formula (try-rules rules current-formula)]
        (recur new-formula)
        current-formula)))
