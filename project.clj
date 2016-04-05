(defproject clondike "0.1.0-SNAPSHOT"
  :description "This project simulates Klondike solitaire in order to determine a good strategy for playing the game."
  :url "https://github.com/StephenRudolph/clondike"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"] [com.rpl/specter "0.9.1"] [clojure-lanterna "0.9.4"]]
  :main ^:skip-aot clondike.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
