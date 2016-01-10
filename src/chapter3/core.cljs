(ns chapter3.core
  (:require [cljs.nodejs :as nodejs]))
(nodejs/enable-util-print!)
(defn -main [& args]
  (println "Hello world!"))
(set! *main-cli-fn* -main)
(defn foo [a b] (+ a b))
(defn bar [a b] (* a b))
