(ns blorg.org
  (:require [blorg.util :refer [vec*]]
            [clojure.walk]))


(defn header-value-for [field txt]
  (->> txt
       (re-seq (->> field
                    (format "\\#\\+%s: (.+?)\n")
                    re-pattern))
       (map second)
       last))


(defn get-title [txt] (header-value-for "TITLE" txt))


(defn get-draft [txt]
  (->> txt (header-value-for "DRAFT") Boolean/valueOf))


(defn get-tags [txt] (header-value-for "TAGS" txt))


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


(defn boldify [s]
  (->> s
       (re-seq #"(?s)((?:(?!\*).)+)?(?:\*((?:(?!\*).)+?)\*)?")
       (remove (partial every? empty?))
       (mapcat (fn [[_ before strong]]
                 (cond
                   (not before) [[:strong strong]]
                   (not strong) [before]
                   :else [before [:strong strong]])))))


(defn emify [s]
  (->> s
       (re-seq #"(?xs)
                 (
                   (?:
                     (?!
                       (?:
                         (?<=\s|^|\")
                         \/
                         ([^\/]+)
                         \/
                       )
                     )
                     .
                   )+
                 )?
                 (?:
                   (?<=\s|^|\")
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


(defn code-ify [s]
  (->> s
       (re-seq #"(?sx)
                 (
                   (?:
                     (?!
                       =
                       (.+?)
                       =
                     )
                     .
                   )+
                 )?
                 (?:
                   =
                   (.+?)
                   =
                 )?")
       (remove (partial every? empty?))
       (mapcat (fn [[_ before code]]
                 (cond
                  (not before) [[:code code]]
                  (not code) [before]
                  :else [before [:code code]])))))


(defn hr-ify [s]
  (->> s
       (re-seq #"(?sx)
                 (
                   (?:
                     (?!
                       (?<=^|\n)
                       -{5,}
                     )
                     .
                   )+
                 )?
                 (
                   (?<=^|\n)
                   -{5,}
                 )?")
      (remove (partial every? empty?))
      (mapcat (fn [[_ before hr]]
                (cond
                 (not before) [[:hr]]
                 (not hr) [before]
                 :else [before [:hr]])))))


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


(defn tree-linkify [tree] (walk-string-fn linkify tree))
(defn tree-boldify [tree] (walk-string-fn boldify tree))
(defn tree-emify [tree] (walk-string-fn emify tree))
(defn tree-code-ify [tree] (walk-string-fn code-ify tree))
(defn tree-hr-ify [tree] (walk-string-fn hr-ify tree))
