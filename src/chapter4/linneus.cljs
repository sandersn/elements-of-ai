(ns chapter4.linneus
  (:refer chapter3.match :only [match match-state atom?])
  (:refer clojure.test :only [with-test is run-tests]))
;TODO: This is probably wrong -- should be locally scoped inside the conversation loop
(def isa (atom {}))
(def includes (atom {}))
(with-test
  (defn add-to-list [aname x d]
    (if (d aname)
      (update-in d [aname] conj x)
      (conj d [aname #{x}])))
  (is (= {'a #{1}} (add-to-list 'a 1 {})))
  (is (= {'a #{1 2}} (add-to-list 'a 2 {'a #{1}}))))
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
(with-test
  (defn article? [article]
    (contains? #{'a 'an 'the 'that 'this 'those 'these} article))
  (is (article? 'a))
  (is (not (article? 'wat))))
(with-test
  (defn interpret [text isa includes article]
    (if-let [d (match '((article? article1) (? x) is (article? article2) (? y)) text)]
      (do
        (println "I understand.")
        ['continue (add-to-list (d 'x) (d 'y) isa) (add-to-list (d 'y) (d 'x) includes) (assoc article (d 'x) (d 'article1) (d 'y) (d 'article2))])
      'bye))
  (is (= '[continue {dog #{animal}} {animal #{dog}} {dog a, animal an}]
         (interpret '(a dog is an animal) {} {} {}))))
(defn linneus []
  (println "This is Linneus. Please talk to me.")
  (loop [isa {} includes {} article {}]
    (println "--> ")
    (let [[result isa includes article] (interpret (read))]
      (if (= result 'bye)
        'goodbye
        (recur isa includes article)))))
