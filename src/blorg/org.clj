(ns blorg.org
  (:require [instaparse.core :refer [parser]]))


(def org-parser
  (parser "document = hdr* body?
           hdr = <'#+'> #'[a-zA-Z]+' <':' #'\\s*'> #'.+' <'\n'>
           body = !hdr #'(?s).+'"))


(defn contents->headers [s]
  (let [terms (->> s org-parser rest)
        hdrs (->> terms
                  (filter (comp (partial = :hdr) first))
                  (map rest)
                  (mapcat (juxt (comp keyword
                                      clojure.string/lower-case
                                      first)
                                second))
                  (apply hash-map))
        body (->> terms
                  (filter (comp (partial = :body) first))
                  first
                  second)]
    (assoc hdrs :body body)))


(defn contents->title [s]
  (-> s
      contents->headers
      :title))
