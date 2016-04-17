(ns chapter4.linneus
  (:refer chapter3.match :only [match match-state atom?])
  (:refer clojure.test :only [with-test is run-tests]))
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
(defn case-match [text cases]
  (loop [cases cases]
    (if (empty? cases)
      (do (println "I do not understand.") ['error {} {} {}])
      (let [[patterns f] (first cases)]
        (if-let [d (some #(match % text) patterns)]
          (f d)
          (recur (rest cases)))))))
(defn chain-interpret [utterances]
  (loop [utterances utterances ds ['start {} {} {}]]
    (if (empty? utterances)
      ds
      (recur (rest utterances)
             (apply interpret (first utterances) (rest ds))))))
(with-test
  (defn make-conj [l article]
    (cond
      (empty? l) nil
      (empty? (rest l)) (cons (article (first l)) l)
      :else (concat (list (article (first l)) (first l) 'and) (make-conj (rest l) article))))
  (is (= nil (make-conj (list) '{dog a})))
  (is (= '(a dog) (make-conj '(dog) '{dog a})))
  (is (= '(a dog and an animal) (make-conj '(dog animal) '{dog a animal an}))))
(defn tell [article x y]
  (list (article x) x 'is (article y) y)) 
(defn explain-chain [isa article x l y]
  (cond
    (empty? l) nil
    (contains? l y) (cons 'and (tell article x y))
    (isa-test isa (first l) y 10)
    (concat (tell article x (first l))
            (explain-chain isa article (first l) (isa (first l)) y))
    :else (explain-chain isa article x (rest l) y)))
(with-test
  (defn explain-links [isa article x y]
    (cond
      (= x y) "They are identical"
      (contains? (isa x) y) "You told me"
      :else (explain-chain isa article x (isa x) y)))
  (is (= "They are identical" (explain-links {} {} 'dog 'dog)))
  (is (= "You told me" (explain-links '{dog #{animal}} '{dog a} 'dog 'animal)))
  (is (= '(a dog is a mammal and a mammal is an animal) (explain-links '{dog #{mammal} mammal #{animal}} '{dog a, mammal a, animal an} 'dog 'animal))))
(with-test
  (defn interpret [text isa includes article]
    (case-match
     text
     [['(((article? article1) (? x) is (article? article2) (? y)))
       (fn [d]
         (println "I understand.") 
         ['add-fact (add-to-list (d 'x) (d 'y) isa) (add-to-list (d 'y) (d 'x) includes) (assoc article (d 'x) (d 'article1) (d 'y) (d 'article2))])]
      ['((what is (? x)) (what is (article? article1) (? x)))
       (fn [d]
         (let [y (or (isa (d 'x)) (includes (d 'x)))
               flag (cond (isa (d 'x)) 'isa (includes (d 'x)) 'includes :else 'dunno)]
           (if (= flag 'dunno)
             (println "I don't know.")
             (println (article (d 'x)) (d 'x)
                      (cond (= flag 'isa) "is" (= flag 'includes) "is something more general than")
                      (make-conj y article)))
           [flag isa includes article]))]
      ['((is (article? article1) (? x) (article? article2) (? y)))
       (fn [d]
         (let [flag
               (if (isa-test isa (d 'x) (d 'y) 10)
                 (do (println "Yes indeed," (article 'x) (d 'x) "is" (article 'y) (d 'y)) 'is-indeed)
                 (do (println "Sorry, not that I know of.") 'is-not))]
           [flag isa includes article]))]
      ['((why is (article? article1) (? x) (article? article2) (? y)))
       (fn [d]
         (let [flag
               (if (isa-test isa (d 'x) (d 'y) 10)
                 (do (println "Because" (explain-links isa article (d 'x) (d 'y))) 'because)
                 (do (println "But it's not!") 'its-not))]
           [flag isa includes article]))]
      ['((bye) (goodbye))
       (fn [d] ['bye isa includes article])]]))
  (is (= '[add-fact
           {dog #{animal}}
           {animal #{dog}}
           {dog a, animal an}]
         (chain-interpret '((a dog is an animal)))))
  (is (= '[add-fact
           {dog #{mammal} mammal #{animal}}
           {mammal #{dog} animal #{mammal}}
           {dog a, animal an, mammal a}]
         (chain-interpret '((a mammal is an animal) (a dog is a mammal)))))
  (is (= '[includes
           {dog #{animal}}
           {animal #{dog}}
           {dog a, animal an}]
         (chain-interpret '((a dog is an animal) (what is an animal))))) 
  (is (= '[dunno
           {dog #{animal}}
           {animal #{dog}}
           {dog a, animal an}]
         (chain-interpret '((a dog is an animal) (what is a wombat)))))
  (is (= '[isa
           {dog #{animal friend}}
           {animal #{dog} friend #{dog}}
           {dog a, animal an, friend a}]
         (chain-interpret '((a dog is an animal) (a dog is a friend) (what is a dog)))))
  (is (= '[because
           {dog #{mammal} mammal #{animal}}
           {mammal #{dog} animal #{mammal}}
           {dog a, animal an, mammal a}]
         (chain-interpret '((a mammal is an animal) (a dog is a mammal) (why is a dog an animal)))))
  (is (= '[its-not
           {dog #{mammal} mammal #{animal}}
           {mammal #{dog} animal #{mammal}}
           {dog a, animal an, mammal a}]
         (chain-interpret '((a mammal is an animal) (a dog is a mammal) (why is a wombat an animal))))))

(defn linneus []
  (println "This is Linneus. Please talk to me.")
  (loop [isa {} includes {} article {}]
    (println "--> ")
    (let [[result isa includes article] (interpret (read))]
      (if (= result 'bye)
        'goodbye
        (recur isa includes article)))))
