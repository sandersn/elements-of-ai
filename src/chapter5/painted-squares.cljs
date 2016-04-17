(ns chapter5.painted-squares
  (:refer chapter3.match :only [match match-state atom?])
  (:refer clojure.test :only [with-test is run-tests]))
(def pattern 
  '{p1 [st ha gr st]
    p2 [bx st ha bx]
    p3 [st ha bx gr]
    p4 [gr gr ha bx]})
(def ^:dynamic *box-width* 2)
(def ^:dynamic *box-length* 2) ; not actually used
(with-test
  (defn rotate-list [l n]
    (let [offset (- (count l) n)]
      (into [] (concat (drop offset l) (take offset l)))))
  (is (= [1 2 3] (rotate-list [1 2 3] 0)))
  (is (= [3 1 2] (rotate-list [1 2 3] 1)))
  (is (= [2 3 1] (rotate-list [1 2 3] 2)))
  (is (= [1 2 3] (rotate-list [1 2 3] 3)))
  (is (= [1 2 3] (rotate-list [1 2 3] 4))) ; should probably wrap around instead
)
(defn orient [piece orientation]
  (rotate-list (pattern piece) (dec orientation)))
(defn sides-ok [new-piece orientation current-state]
  (if (empty? current-state)
    true
    (let [trial (orient new-piece orientation)
          len (count current-state)]
      (cond
        (= 0 (rem len *box-width*)) (return (match-north trial current-state))
        (< len *box-width*) (return (match-west trial current-state))
        :else (return (and (match-north trial current-state)
                           (match-west trial current-state)))))))
(defn match-north [trial state]
  (= (trial 2) ;north side
     ((orient (state (dec *box-width*))) 0)))
(defn match-west [trial state]
  (= (trial 3) ; west side of rotated new piece
     ((orient (first state)) 2)))
        

(defn main []
  (loop [pieces '#{p1 p2 p3 p4}]
    'p1))
         
            
