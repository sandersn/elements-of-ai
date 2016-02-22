(ns chapter3.leibniz
  (:refer chapter3.match :only [match match-state atom?]))
(defn nested-diff-sum? [d v]
  (and (not (atom? v)) (match '(+ (? e1) (? e2)) v)))
(defn nested-diff-x1? [d v]
  {:e1 v})
(defn nested-diff-x2? [d v]
  (= v (d :e1)))
(def diff-sum-rule
  ['(d (nested-diff-sum? e3) (? v1))
   #(list '+ (list 'd (% :e1) (% :v1)) (list 'd (% :e2) (% :v1)))
   "diff-sum-rule"])
(def diff-x-rule
  ['(d (nested-diff-x1? e1) (nested-diff-x2? e2))
   1
   "diff-x-rule"])
; other rules go here ...

; TODO: Clojure HAS to have a class/record facility to do this...
(def rule-pattern first)
(defn rule-action [rule] second)
(defn rule-name [rule] (nth rule 2))

(def rules
  [[diff-sum-rule diff-x-rule]
   [; TODO: arithmetic simplication rules go here
    ]])
   
;;; control scheme ;;;
; (I don't know--that's just what the book calls it)
(declare try-rule)
(defn try-rule-on-list [rule expressions]
  (if (empty? expressions)
    nil
    (if-let [temp (try-rule rule (first expressions))]
      (cons temp (rest expressions))
      (when-let [temp (try-rule-on-list rule (rest expressions))]
        (cons (first expressions) temp)))))
(defn fire [rule d]
  (println (rule-name rule) " fires.")
  ((rule-action rule) d))
(defn try-rule [rule expression]
  (if (atom? expression)
    nil
    (if-let [d (match-state (rule-pattern rule) expression)]
      (fire rule d)
      (try-rule-on-list rule expression))))
(defn try-rules [rules formula]
  (loop [rules-left rules
         formula formula]
    (cond
      (empty? rules-left) nil ; no rules applied -- fail 
      (atom? formula) nil ; ??? (reduced to an atom?) -- fail
      :else (if-let [new-formula (try-rule (first rules-left) formula)]
              new-formula
              (recur (rest rules-left) formula)))))
(defn control [formula rules]
  (loop [formula formula
         rules rules]
    (if-let [new-formula (try-rules (first rules) formula)]
      (recur new-formula rules)
      (if (empty? (rest rules))
        formula
        (recur formula (rest rules))))))
