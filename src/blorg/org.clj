(ns blorg.org
  (:require [blorg.util :refer [vec*]]
            [clojure.walk]
            [hiccup.util :refer [escape-html]]
            [instaparse.core :refer [parser transform get-failure]]))


(defn get-title [txt]
  (second (re-find #"\#\+TITLE: (.+?)\n" txt)))


(defn get-draft [txt]
  (->> txt
       (re-find #"\#\+DRAFT: (.+?)\n")
       second
       Boolean/valueOf))


(defn get-tags [txt]
  (->> txt
       (re-find #"\#\+TAGS: (.+?)\n")
       second))


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
           body = (newline? #'[^\n]+' !newline) | ((body-line)+ !body)"))


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
  (parser "<D>        = txt
           <txt>      = (nonlink | link)+
           <nonlink>  = (words | em | strong)+
           star       = '*'
           slash      = '/'
           url        = ('http'|'https') '://' #'(?!\\])\\S+'
           em         = <slash> #'[^/]+' <slash>
           <nolnkbld> = #'(?s)((?!\\[)[^\\*])+'
           strong     = <star> (nolnkbld | link)+ <star>
           link       = link1 | link2
           ll         = '[['
           lr         = ']['
           rr         = ']]'
           <linkbody> = !strong #'(?s)((?!\\]|\\*).)*'
           <link1>    = !link2 <ll> linkbody <rr>
           <link2>    = <ll> linkbody <lr> (linkbody|strong)+ <rr>
           <words>    = !ll #'(?xs)
                              ( # Lookaheads:
                                (?!\\*.+\\*)  # Not a `strong`
                                (?!/.+/)      # Not an `em`
                                (?!\\[\\[)    # Not starting a comment
                                (?!\\]\\])    # Not ending a comment
                              .)+'"))


(defn check-parse [orig parsed]
  (if (get-failure parsed)
    [:pre "parse FAILED:" orig]
    parsed))


(defn as-hiccup [parsed]
  (transform
   {:body #(->> %&
                (apply str)
                body-parser
                (check-parse %&))
    :hdr (fn [& args] "")

    :section-header
    (fn [& args]
      (let [section-title (last args)
            tag (->> args count dec (str "h") keyword)]
        [tag section-title]))

    :section
    (fn [& args]
      (list* :div args))}
   parsed))


(defn xform-paragraphs [parsed]
  (transform
   {:p (fn [& [z & zz]]
         (when zz (println "EXTRA STUFF:" zz "(before" z ")"))
         (->> z
              paragraph-parser
              (check-parse z)
              (vec* :p)))
    :h1 (fn [z]
          (->> z
               paragraph-parser
               (check-parse z)
               (vec* :h1)))}
   parsed))


(defn xform-links [parsed]
  (transform
   {:link (fn [& [a & rst]]
            (if (seq rst)
              (vec* :a {:href a} rst)
              [:a {:href a} a]))}
   parsed))


(defn parse-stages [raw-text]
  (let [first-parse (org-parser raw-text)
        into-paragraphs (as-hiccup first-parse)
        with-markup (xform-paragraphs into-paragraphs)
        with-links (xform-links with-markup)]
    {:raw-text raw-text
     :first-parse first-parse
     :into-paragraphs into-paragraphs
     :with-markup with-markup
     :with-links with-links}))


(defn strip-raw-html-tags-for-now [txt]
  (clojure.string/replace txt #"#\+HTML:.+?\n" ""))


(defn split-headers-and-body [txt]
  (let [r (re-find #"(?x)
                     (\n*     # Pick up trailing newlines
                      (?:\#   # Anything starting w/ '#'
                       (?:    # Not starting with:
                        (?!\+(?:HTML:|CAPTION|BEGIN|ATTR_HTML))
                              # Swallow all lines that match
                        .)+\n*)*)
                              # Swallow everything else as group 2
                     ((?s)(?:.+)*)"
                   txt)]
    (rest r)))


(defn convert-body-to-sections [body]
  (let [matches
        (re-seq #"(?x)
                  (?:
                   (?:(\*+)\s+(.+)\n)
                   |
                   ((?:(?!\*+\s+).*\n)*)
                  )(?x)"
                body)]
    (->> (for [[_ stars hdr body] matches]
           (if stars
             [(-> stars
                  count
                  ((partial str "h"))
                  keyword)
              hdr]
             body))
         (remove #{""})
         (vec* :div))))


(defn find-paragraphs [s]
  (->> s
       (re-seq #"(?x)
                 ((?:.+\n?)+)
                 |
                 (?:\n{2,})")
       (map (comp (partial vec* :p) rest))))


(defn section-bodies-to-paragraphs [tree]
  (letfn [(convert-paragraphs-leave-hn [term]
            (if (string? term)
              (find-paragraphs term)
              [term]))]
    (->> tree
         (mapcat convert-paragraphs-leave-hn)
         vec)))


(defn linkify [s]
  (->> s
       (re-seq #"(?s)((?:(?!\[\[).)+)?(?:\[\[(.+?)\]\[(.+?)\]\])?")
       (remove (partial every? empty?))
       (mapcat (fn [[_ before lnk body]]
                 (cond
                  (not before) [[:a {:href lnk} body]]
                  (not lnk) [before]
                  :else [before [:a {:href lnk} body]])))))


(defn walk-string-fn
  "
  Walk tree, applying f to each string.  If multiple terms result, put
  them inside a :span tag.
  "
  [f tree]
  (clojure.walk/postwalk (fn [el]
                           (if (string? el)
                             (let [[r0 & rs :as r] (f el)]
                               (if rs
                                 (vec* :span r)
                                 r0))
                             el))
                         tree))


(defn tree-linkify [tree]
  (walk-string-fn linkify tree))


(defn boldify [s]
  (->> s
       (re-seq #"(?s)((?:(?!\*).)+)?(?:\*((?:(?!\*).)+?)\*)?")
       (remove (partial every? empty?))
       (mapcat (fn [[_ before strong]]
                 (cond
                   (not before) [[:strong strong]]
                   (not strong) [before]
                   :else [before [:strong strong]])))))


(defn tree-boldify [tree]
  (walk-string-fn boldify tree))


(defn emify [s]
  (->> s
       (re-seq #"(?xs)
                 (
                   (?:
                     (?!
                       (?:
                         (?<=\s|^)
                         \/
                         ([^\/]+)
                         \/
                       )
                     )
                     .
                   )+
                 )?
                 (?:
                   (?<=\s|^)
                   \/
                   ([^\/]+)
                   \/
                 )?")
       (remove (partial every? empty?))
       (mapcat (fn [[_ before em]]
                 (cond
                   (not before) [[:em em]]
                   (not em) [before]
                   :else [before [:em em]])))))


(defn tree-emify [tree]
  (walk-string-fn emify tree))
