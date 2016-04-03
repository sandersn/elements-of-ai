(ns chapter3.leibniz
  (:refer chapter3.match :only [match match-state atom?])
  (:refer clojure.test :only [with-test is run-tests]))
(defn nested-diff-sum? [d v]
  (and (not (atom? v)) 
       (when-let [d2 (match-state '(+ (? e1) (? e2)) v)]
         (merge d d2))))
(def diff-sum-rule
  ['(d (nested-diff-sum? e3) (? v1))
   #(list '+ (list 'd (% 'e1) (% 'v1)) (list 'd (% 'e2) (% 'v1)))
   "diff-sum-rule"])
(defn nested-diff-x1? [d v] d)
(defn nested-diff-x2? [d v] (and (= v (d 'e1)) d))
(def diff-x-rule
  ['(d (nested-diff-x1? e1) (nested-diff-x2? e2))
   (fn [d] 1)
   "diff-x-rule"])
(with-test
  (defn exists-tree? [f v1]
    (if (atom? f)
      (= f v1)
      (some (fn [x] 
              (if (atom? x)
                (= x v1)
                (exists-tree? x v1)))
            f)))
  (is (exists-tree? '(1 2 3) 1))
  (is (not (exists-tree? '(1 2 3) 4)))
  (is (not (exists-tree? '((2 3) (4 (5) 3) 2 2) 1)))
  (is (exists-tree? '((2 1) (4 (5) 3) 2 2) 1))
  (is (exists-tree? '((2 3) (4 (1) 3) 2 2) 1))
  (is (exists-tree? '((2 3) (4 (5) 3) 2 1) 1)))
(defn diff-const-e2-rule [d x] (and (not (exists-tree? (d 'e1) x)) d))
(def diff-const-rule
  ['(d (? e1) (diff-const-e2-rule e2))
   (fn [d] 0)
   "diff-const-rule"])
(defn diff-product-e3-rule [d x]
  (and (not (atom? x))
       (when-let [d2 (match-state '(* (? e1) (? e2)) x)]
         (merge d d2))))
(def diff-product-rule
  ['(d (diff-product-e3-rule e3) (? v1))
   (fn [d] (list '+
                 (list '* (d 'e2) (list 'd (d 'e1) (d 'v1)))
                 (list '* (d 'e1) (list 'd (d 'e2) (d 'v1)))))
   "diff-product-rule"])
(defn number-state? [d x]
  (when (number? x) d))
(defn diff-power-e3-rule [d x]
  (and (not (atom? x))
       (when-let [d2 (match-state '(exp (? e1) (number-state? e2)) x)]
         (merge d d2))))
(def diff-power-rule
  ['(d (diff-power-e3-rule e3) (? v1))
   (fn [d] (list '* (d 'e2)
                 (list '* (list 'exp (d 'e1) (list 'dec (d 'e2)))
                       (list 'd (d 'e1) (d 'v1)))))
   "diff-power-rule"])
; other rules go here ...

; TODO: Clojure HAS to have a class/record facility to do this...
(def rule-pattern first)
(def rule-action second)
(defn rule-name [rule] (nth rule 2))

(def rules
  [[diff-sum-rule diff-x-rule diff-const-rule diff-product-rule diff-power-rule]
   [; TODO: arithmetic simplification rules go here
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
  (println (rule-name rule) "fires.")
  ((rule-action rule) d))
(defn try-rule [rule expression]
  (if (atom? expression)
    nil
    (if-let [d (match-state (rule-pattern rule) expression)]
      (fire rule d)
      (try-rule-on-list rule expression))))
(with-test
  (defn try-rules [rules formula]
    (loop [rules-left rules
           formula formula]
      (cond
        (empty? rules-left) nil ; no rules applied -- fail 
        :else (if-let [new-formula (try-rule (first rules-left) formula)]
                new-formula
                (recur (rest rules-left) formula)))))
  (is (not (try-rules '() '(d (* 2 x) x))))
  (is (= '(+ (d (exp x 2) x) (+ (* x (d 2 x)) (* 2 1)))
         (try-rules (first rules) '(+ (d (exp x 2) x) (+ (* x (d 2 x)) (* 2 (d x x))))))))

(with-test
  (defn control [formula rules]
    (loop [formula formula
           rules rules]
      (if-let [new-formula (try-rules (first rules) formula)]
        (recur new-formula rules)
        (if (empty? rules)
          formula
          (recur formula (rest rules))))))
  (is (= 0 (control '(d 2 x) rules)))
  (is (= 1 (control '(d x x) rules)))
  (is (= '(+ (* x 0) (* 2 1)) (control '(d (* 2 x) x) rules)))
  (is (= '(+ (* x 0) (* 2 1)) (control '(+ (* x (d 2 x)) (* 2 1)) rules)))
  (is (= '(+ (* x 0) (* 2 1)) (control (control '(d (* 2 x) x) rules) rules)))
  (is (= '(+ (* 2 (* (exp x (dec 2)) 1)) (+ (* x 0) (* 2 1))) (control '(d (+ (exp x 2) (* 2 x)) x) rules)))
  )
