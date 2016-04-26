(ns clondike.core
  (:gen-class)
  (:require [clojure.core.async :refer (chan)]
            [clondike.app :as app]
            [clondike.ui.cli :as cli]
            [com.stuartsierra.component :as component]
            [clojure.tools.namespace.repl :refer (refresh)]))

(defn cli-system [config-options]
  (let [{:keys [cols rows]} config-options]
    (-> (component/system-map
          :sc (cli/map->Screen {:cols cols :row rows})
          :app (app/map->App {})
          :cc (chan))
        (component/system-using
          {:sc {:command-channel :cc}
           :app {:screen :sc :command-channel :cc}}))))

(def system nil)

(defn init []
  (alter-var-root #'system
                  (constantly (cli-system {:cols 80 :rows 24}))))

(defn start []
  (alter-var-root #'system component/start))

(defn stop []
  (alter-var-root #'system
                  (fn [s] (when s (component/stop s)))))

(defn go []
  (init)
  (start))

(defn reset []
  (stop)
  (refresh :after 'user/go))

(defn -main
  "Start a new game of Clondike"
  [& args]
  (component/start (cli-system {:cols 80 :rows 24})))