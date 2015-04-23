(ns blorg.mac
  "
  Mac-specific namespace.  FIXME: handle other platforms gracefully.
  "
  (:require [clojure.java.shell :refer [sh]]))


(defn say [s]
  (->> s
       (format "say %s")
       (#(clojure.string/split % #" "))
       (apply sh)))
