(ns blorg.watcher
  (:require [watchtower.core :refer :all]))


(defn start-watcher [paths watch-function]
  (watcher paths
    (rate 300)
    (file-filter (extensions :org :clj))
    (on-change watch-function)))
