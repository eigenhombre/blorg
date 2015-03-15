(ns blorg.util)


(defn pluralize [n s]
  (format "%s%s" s (if (= n 1) "" "s")))
