(defproject clondike "0.1.0-SNAPSHOT"
  :description "This project is a smart, helpful, command-line Klondike solitaire implementation."
  :url "https://github.com/StephenRudolph/clondike"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [com.rpl/specter "0.10.0"]
                 [clojure-lanterna "0.9.4"]
                 [com.stuartsierra/component "0.3.1"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [org.clojure/core.async "0.2.374"]]
  :main ^:skip-aot clondike.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
