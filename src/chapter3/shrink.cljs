(ns chapter3.shrink
  (:refer chapter3.match :only [match]))
(defn wword [] (rand-nth '[when why where how]))
(defn punt [] (rand-nth '[(please go on)
                          (tell me more)
                          (i see)
                          (what does that indicate)
                          (but why be concerned about it)
                          (just tell me how you feel)]))
(def you-me-map
  {'i 'you
   'me 'you
   'you 'me
   'my 'your
   'your 'my
   'yours 'mine
   'mine 'yours
   'am 'are})
(def you->me (partial map #(or (you-me-map %) %)))
(defn printl [& l] (println (apply concat l)))
(def w-word '#{why where when what which how})
(def modal-word '#{do can should would})
(def verb 
  '#{go have be try eat take help make get jump write type fill put turn
     compute think drink blink crash crunch add})
(def knowledge
  {'(bye) (fn [x] 'goodbye)
   '(you are (* x)) #(printl '(please tell me) (list (wword)) '(you are) (% 'x))
   '(you have (* x)) #(printl '(how long have you had) (% 'x))
   '(you feel (* x)) (fn [m] (println '(i sometimes feel the same way)))
   '(because (* x)) (fn [m] (println '(is that really the reason)))
   '() (fn [m] (println '(please say something!)))
   '(yes (* x)) (fn [m] (println '(how can you be so sure)))
   '(me are (* x)) #(printl '(oh yeah i am) (% 'x))
   '((verb v) (* x)) #(printl '(why do you want me to (list (% 'v)) (% 'x)))
   '((w-word w) (* x)) #(printl '(you tell me) (list (% 'w)) (% 'x))
   '((modal-word w) me (* x)) #(printl '(perhaps i) (list (% 'w)) (% 'x))
   '(do me think (* x)) (fn [m] (println "I think you should answer that for yourself"))
  })
(defn mentions? [w l]
  (some #(= w %) l))

(defn choose-first
  [f l]
  (loop [[key & keys] (seq l)]
    (if key
      (if-let [value (f key)]
        [key value] 
        (recur keys)))))
(defn shrink []
  (println "Welcome to my sofa!") 
  (loop []
    (let [s (you->me (read))]
      (if-let [[pattern matches] (choose-first #(match % s) (keys knowledge))]
        (if-let [result ((knowledge pattern) matches)]
          result
          (recur))
        (do
          (cond
            (mentions? 'dream s) (println "For dream analysis see Freud.")
            (mentions? 'love s) (println "All is fair in love and war.")
            (mentions? 'no s) (println "Don't be so negative.")
            (mentions? 'maybe s) (println "Be more decisive!")
            (mentions? 'you s) (println s)
            :else (printl (punt)))
          (recur))))))
