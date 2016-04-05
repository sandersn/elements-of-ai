(ns chapter4.linneus
  (:refer chapter3.match :only [match match-state atom?])
  (:refer clojure.test :only [with-test is run-tests]))
;TODO: This is probably wrong -- should be locally scoped inside the conversation loop
(def isa (atom {}))
(def includes (atom {}))
(with-test
  (defn add-to-list [aname x]
    (fn [d]
      (if (d aname)
        (update-in d [aname] conj x)
        (conj d [aname #{x}]))))
  (is (= {'a #{1}} ((add-to-list 'a 1) {})))
  (is (= {'a #{1 2}} ((add-to-list 'a 2) {'a #{1}}))))
(defn add-superset [aname x]
  (swap! isa (add-to-list aname x)))
(defn add-subset [aname x]
  (swap! includes (add-to-list aname x)))
(with-test
  (defn isa-test [isa x y n]
    (if (zero? n)
      false
      (or (= x y)
          (contains? (isa x) y)
          (some (fn [xx] (isa-test isa xx y (dec n))) (isa x)))))
  (is (isa-test {} 'dog 'dog 100))
  (is (not (isa-test {} 'dog 'cat 100)))
  (is (isa-test {'dog #{'mammal}} 'dog 'mammal 100))
  (is (not (isa-test {'d #{'mammal}} 'dog 'bug 100)))
  (is (isa-test {'dog #{'mammal} 'mammal #{'animal}} 'dog 'animal 2))
  (is (not (isa-test {'dog #{'mammal} 'mammal #{'animal}} 'dog 'animal 1))))
