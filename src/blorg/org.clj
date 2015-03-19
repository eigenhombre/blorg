(ns blorg.org)


(defn extract-title-from-contents [s]
  (->> s
       (re-find #"\#\+TITLE: (.+)")
       second))
