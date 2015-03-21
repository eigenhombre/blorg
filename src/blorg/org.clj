(ns blorg.org
  (:require [instaparse.core :refer [parser transform]]))


(def org-parser
  (parser "document = (hdr|newline|comment)* body? section*
           <newline> = '\n'
           comment = '# ' #'[^\n]*' <'\n'>
           hdr = <'#+'> #'[a-zA-Z_]+' <':'> <#' *'> #'[^\n]*' <newline>
           section-header = #'\\*+' <#' +'> #'[^\n]*' <newline>
           section = section-header body?
           <nonstar-first-bit> = #'(?!\\* ).'
           <any-char> = #'.*'
           <non-section-line> = nonstar-first-bit any-char newline
           <body-line> = !section-header !hdr !comment non-section-line
           body = (body-line | newline | <comment>)+"))


(defn txform [tree]
  (transform {:body (fn [& terms] [:body (apply str terms)])} tree))


(defn contents->headers [s]
  (let [terms (->> s org-parser txform rest)
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
                  (filter (comp #{:body :section} first)))]
    (assoc hdrs :body body)))
