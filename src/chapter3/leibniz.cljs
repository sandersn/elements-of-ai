(ns chapter3.leibniz
  (:refer chapter3.match :only [match atom?]))
(defn nested-diff-sum? [d v]
  (when-let [d2 (and (not (atom? v)) (match '(+ (? e1) (? e2)) v))]
    (merge d d2)))
(defn nested-diff-x1? [d v]
  (assoc d :e1 v))
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

(def rules
  [diff-sum-rule diff-x-rule])
;;; control scheme ;;;
; (I don't know--that's just what the book calls it)
(defn control []
  ; TODO: current-formula is unbound
  (loop [current-formula '()]
      (if-let [new-formula (try-rules rules current-formula)]
        (recur new-formula)
        current-formula)))
(defn try-rules [rules formula]
  (loop [rules-left rules]
    (cond
      (empty? rules-left) nil ; no rules applied -- fail 
      (atom? formula) nil ; ??? (reduced to an atom?) -- fail
      :else (if-let [new-formula (try-rule (first rules-left) formula)]
              new-formula
              (recur (rest rules-left) formula)))))
(defn try-rule [rule expression]
  (if (= current-goal (rule-goal rule))
    (try-rule1 rule expression)))
(defn try-rule1 [rule expression]
  (cond
    (atom? expression) nil
    (match-state (rule-pattern rule) expression) (fire rule)
    :else (try-rule-on-list rule expression)))
(defn try-rule-on-list [rule expressions]
  (if (empty? expression-list)
    nil
    (if-let [temp (try-rule1 rule (first expression-list))]
      (cons temp (rest expression-list))
      (when-let [temp (try-rule-on-list rule (rest expression-list))]
        (cons (first expression-list) temp)))))
(defn fire [rule]
  (printfn (rule-name rule) " fires.")
  ((rule-action rule)))
