(ns clondike.core
  (:gen-class)
  (:require [clojure.core.async :refer (chan <!!)]
            [clondike.app :as app]
            [clondike.ui.cli :as cli]
            [com.stuartsierra.component :as component]
            [clojure.tools.namespace.repl :refer (refresh)]))

(defn cli-system [stop-chan config-options]
  (let [{:keys [cols rows screen]} config-options]
    (-> (component/system-map
          :sc (cli/map->Screen {:cols cols :rows rows :screen-type screen})
          :app (app/map->App {:stop-chan stop-chan})
          :cc (chan))
        (component/system-using
          {:sc {:command-channel :cc}
           :app {:screen :sc :command-channel :cc}}))))

(def system nil)

(defn init []
  (alter-var-root #'system
                  (constantly (cli-system (chan) {:cols 80 :rows 24 :screen :swing}))))

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
  (let [stop-chan (chan)]
    (if (= "-t" (first args))
      (component/start (cli-system stop-chan {:cols 80 :rows 24 :screen :text}))
      (component/start (cli-system stop-chan {:cols 80 :rows 24 :screen :swing})))
    (<!! stop-chan)))