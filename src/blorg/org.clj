(ns blorg.org
  (:require [hiccup.util :refer [escape-html]]
            [instaparse.core :refer [parser transform]]))


(def org-parser
  (parser "document = (section | hdr | comment | body)*
           <newline> = '\n'
           <comment> = <#'# [^\n]*\n'>
           hdr = <'#+'> #'[a-zA-Z_]+' <#': *'> #'[^\n]*' <newline>+
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


(def body-parser
  (parser "div = <nl>? (p <nl+>)* p <nl?> | <nl>+
           <nl> = '\n'
           p = !nl #'((?s)(?!\n\n).)+\n?'"))


(def paragraph-parser
  "
  Parser for things within paragraphs
  "
  (parser "<D>     = txt
           <txt>   = (words | em | strong | link)+
           em      = <'/'> #'[^/]+' <'/'>
           strong  = <'*'> #'[^\\*]+' <'*'>
           link    = <'[['> #'(?s)((?!\\]\\]).)*' <']]'>
           <words> = !'[[' #'(?s)((?!\\*.+\\*)(?!/.+/)(?!\\[\\[).)+'"))


(def as-hiccup
  (partial transform
   {:body #(->> %&
                (apply str)
                body-parser)
    :hdr (fn [& args] "")

    :section-header
    (fn [& args]
      (let [section-title (last args)
            tag (->> args count (+ -2) (str "h") keyword)]
        [tag section-title]))

    :section
    (fn [& args]
      args)}))


(def xform-paragraphs
  (partial
   transform
   {:p (fn [x]
         (into [] (list* :p (paragraph-parser x))))}))
