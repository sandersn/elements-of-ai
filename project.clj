(defproject elements-of-ai "0.1.0-SNAPSHOT"
  :description "Example implementations from Elements of Artificial Intelligence by Steven Tanimoto"
  :url "https://github.com/sandersn/elements-of-ai"
  :license {:name "MIT"
            :url "https://github.com/sandersn/elements-of-ai/blob/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.7.0"]]
  :main ^:skip-aot chapter3.match
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
