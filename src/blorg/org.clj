(ns blorg.org
  (:require [instaparse.core :refer [parser transform]]))


(def org-parser
  (parser "document = (section | hdr | comment | body)*
           <newline> = '\n'
           <comment> = <#'# [^\n]*\n'>
           hdr = <'#+'> #'[a-zA-Z_]+' <#': *'> #'[^\n]*' <newline>
           section-header = starspace #'[^\n]*' <newline>
           section = section-header body? !body
           <starspace> = '*'+ <' '+>
           <non-section-line> = !starspace #'.*\n'
           <body-line> = !section-header !hdr !comment non-section-line
           body = (body-line)+ !body"))


(defn get-last-tag-value [parsed tagname]
  (->> parsed
       rest
       (filter (comp (partial = :hdr) first))
       (filter (comp (partial = tagname) second))
       last
       last))


(defn doc-title [parsed]
  (get-last-tag-value parsed "TITLE"))


(defn doc-tags [parsed]
  (get-last-tag-value parsed "TAGS"))


(defn doc-draft [parsed]
  (-> parsed
      (get-last-tag-value "DRAFT")
      Boolean/valueOf))
