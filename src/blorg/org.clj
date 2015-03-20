(ns blorg.org
  (:require [instaparse.core :refer [parser]]))


(def org-parser
  (parser "document = (hdr|newline|comment)* body? section*
           newline = '\n'
           comment = '# ' #'[^\n]*' <'\n'>
           hdr = <'#+'> #'[a-zA-Z_]+' <':'> <#' *'> #'[^\n]*' <'\n'>
           section-header = #'\\*+' <#' *'> #'[^\n]*' <'\n'>
           section = section-header body?
           body = !section-header !hdr #'[^\\*]*'"))


(defn contents->headers [s]
  (let [terms (->> s org-parser rest)
        hdrs (->> terms
                  (filter (comp (partial = :hdr) first))
                  (map rest)
                  (remove (comp (partial = "LaTeX_HEADER") first))
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
