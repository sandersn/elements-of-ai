(ns chapter5.painted-squares
  (:refer chapter3.match :only [match match-state atom?])
  (:refer clojure.test :only [with-test is run-tests]))
                                        ; type Piece = [Symbol]
                                        ; type Orientation = Int
                                        ; type Placement = (Symbol, Orientation)
                                        ; type State = [Placement]
                                        ; type Patterns = Map Symbol Piece
(def pattern ; pattern :: Patterns
  '{p1 [st ha gr st]
    p2 [bx st ha bx]
    p3 [st ha bx gr]
    p4 [gr gr ha bx]})
(def box-width 2)
(def box-length 2) ; not actually used
(def south 0)
(def east 1)
(def north 2)
(def west 3)

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
(with-test
  (defn orient [[piece orientation]]
    "This is pretty much only needed because of map lookup for the piece's symbol.
Note that orientation is 1-based on the book, but 0-based here."
    (rotate-list (pattern piece) orientation))
  (is (= '[st ha gr st] (orient '(p1 0))))
  (is (= '[st st ha gr] (orient '(p1 1)))))
(with-test
  (defn match-north [trial state]
    "(nth state (dec box-width)) looks like exactly one row up"
    (= (trial north)
       ((orient (nth state (dec box-width))) south)))
  (is (match-north '[-- -- st --] '((-- 2) (p1 0))))
  (is (match-north '[-- -- gr --] '((-- 2) (p1 2))))
  (is (not (match-north '[-- -- st --] '((-- 2) (p1 2))))))
(with-test
  (defn match-west [trial state]
    (= (trial west)
       ((orient (first state)) east)))
  (is (match-west '[st st st st] '((p1 1))))
  (is (match-west '[ha ha ha ha] '((p1 0))))
  (is (not (match-west '[st st st st] '((p1 0) (-- 2) (-- 3)))))
  (is (not (match-west '[ha ha ha ha] '((p1 1)))))
  (is (match-west '[-- -- -- st] '((p1 1))))
  (is (match-west '[-- -- -- ha] '((p1 0))))
)
(with-test
  (defn sides-ok [placement current-state]
    (let [trial (orient placement)
          len (count current-state)]
      (cond
                                        ;  So he's checking whether we're currently in the (1) leftmost column (2) first row (3) other.
                                        ; there are really only four cases in this setup, but this method will extend to any size at all.
        ; first entry -- no check needed
        (= 0 len) true
        ; first column -- check to the north (but not west)
        (= 0 (rem len box-width)) (match-north trial current-state)
        ; first row -- check to the west (but not north)
        (< len box-width) (match-west trial current-state)
        ; others -- check both north and west
        :else (and (match-north trial current-state)
                   (match-west trial current-state)))))
  (is (sides-ok '(p1 0) '()))
  (is (sides-ok '(p2 1) '()))
  (is (sides-ok '(p1 0) '((-- 0) (p3 1))))
  (is (sides-ok '(p1 0) '((-- 0) (p4 0))))
  (is (sides-ok '(p1 0) '((p3 1))))
  (is (sides-ok '(p1 0) '((p2 0))))
  (is (sides-ok '(p1 0) '((p2 0) (p4 0) (-- 0))))
)
(defn main []
  (loop [pieces '#{p1 p2 p3 p4}]
    'p1))
         
            
