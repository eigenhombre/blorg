(ns blorg.org
  (:require [blorg.util :refer [vec* selective-walk]]
            [hiccup.util :refer [escape-html]]
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


(defn ^:private descend? [el]
  (and (vector? el)
       (not= :pre (first el))))


(defn apply-fn-to-strings
  "
  Walk tree, applying f to each string.  If multiple terms result, put
  them inside a :span tag.
  "
  [f tree]
  (let [f (fn [el]
            (let [[r0 & rs :as r] (f el)]
              (if rs
                (vec* :span r)
                r0)))]
    (selective-walk f descend? string? tree)))


(defn split-headers-and-body [txt]
  (let [[_ & rs]
        (re-find #"(?x)
                   (\n*     # Pick up trailing newlines
                    (?:\#   # Anything starting w/ '#'
                     (?:    # Not starting with:
                      (?!\+(?:HTML:|CAPTION|BEGIN|ATTR_HTML))
                            # Swallow all lines that match
                      .)+\n*)*)
                            # Swallow everything else as group 2
                   ((?s)(?:.+)*)"
                        txt)]
    rs))


(defn convert-body-to-sections [body]
  (let [matches
        (re-seq #"(?x)
                  (?:
                    (?:
                      (\*+)
                      \s+
                      (.+)
                      \n
                    )|
                    (
                      (?:
                        (?!\*+\s+)
                        .*\n
                      )*
                    )
                  )"
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


(defn captionify [s]
  (->> s
       (re-seq #"(?sx)
                 (
                   (?:
                     (?!\[\[)
                     .
                   )+
                 )?
                 (?:
                   \[\[
                   (.+?)
                   \]\]
                 )?")
       (remove (partial every? empty?))
       (mapcat (fn [[_ before img]]
                 (cond
                   (not before) [[:a {:href img} [:img {:src img
                                                        :class "caption"}]]]
                  (not img) [before]
                  :else [before [:a {:href img} [:img {:src img
                                                       :class "caption"}]]])))))


(defn linkify [s]
    (->> s
       (re-seq #"(?sx)
                 (
                   (?:
                     (?!
                       \[\[.+?\]\[.+?\]\]
                     )
                     .
                   )+
                 )?
                 (?:
                   \[\[
                   (.+?)
                   \]\[
                   (.+?)
                   \]\]
                 )?")
       (remove (partial every? empty?))
       (mapcat (fn [[_ before lnk body]]
                 (cond
                  (not before) [[:a {:href lnk} body]]
                  (not lnk) [before]
                  :else [before [:a {:href lnk} body]])))))


(defn boldify [s]
  (->> s
       (re-seq #"(?sx)
                 (
                   (?:
                     (?!\*)
                     .
                   )+
                 )?
                 (?:
                   \*
                   (
                     (?:
                       (?!\*)
                       .
                     )+?
                   )
                   \*
                 )?")
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


(defn strike-ify [s]
    (->> s
       (re-seq #"(?sx)
                 (
                   (?:
                     (?!
                       \+
                       (.+?)
                       \+
                     )
                     .
                   )+
                 )?
                 (?:
                   \+
                   (.+?)
                   \+
                 )?")
       (remove (partial every? empty?))
       (mapcat (fn [[_ before strike]]
                 (cond
                  (not before) [[:strike strike]]
                  (not strike) [before]
                  :else [before [:strike strike]])))))


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


(defn srcify [txt]
  (->> txt
       (re-seq #"(?xs)
                 (
                   (?:
                     (?!
                       \#\+BEGIN_SRC\s+
                       \S+
                       \n
                       .+?
                       \#\+END_SRC\n
                     )
                     .
                   )+
                 )?
                 (?:
                   \#\+BEGIN_SRC\s+
                   (\S+)
                   \n
                   (.+?)
                   \#\+END_SRC\n
                 )?")
       (remove (partial every? empty?))
       (mapcat (fn [[_ before lang block]]
                 (cond
                  (not before) [[:pre {:class (str "lang_" lang)} block]]
                  (not block) [before]
                  :else [before [:pre {:class (str "lang_" lang)} block]])))))


(defn example-ify [txt]
  (->> txt
       (re-seq #"(?xs)
                 (
                   (?:
                     (?!
                       \#\+BEGIN_EXAMPLE\n
                       .+?
                       \#\+END_EXAMPLE\n
                     )
                     .
                   )+
                 )?
                 (?:
                   \#\+BEGIN_EXAMPLE\n
                   (.+?)
                   \#\+END_EXAMPLE\n
                 )?")
       (remove (partial every? empty?))
       (mapcat (fn [[_ before block]]
                 (cond
                  (not before) [[:pre (escape-html block)]]
                  (not block) [before]
                  :else [before [:pre (escape-html block)]])))))


(defn tree-linkify [tree] (apply-fn-to-strings linkify tree))
(defn tree-captionify [tree] (apply-fn-to-strings captionify tree))
(defn tree-boldify [tree] (apply-fn-to-strings boldify tree))
(defn tree-emify [tree] (apply-fn-to-strings emify tree))
(defn tree-code-ify [tree] (apply-fn-to-strings code-ify tree))
(defn tree-strike-ify [tree] (apply-fn-to-strings strike-ify tree))
(defn tree-hr-ify [tree] (apply-fn-to-strings hr-ify tree))
(defn tree-srcify [tree] (apply-fn-to-strings srcify tree))
(defn tree-example-ify [tree] (apply-fn-to-strings example-ify tree))
(defn tree-pars [tree] (apply-fn-to-strings find-paragraphs tree))


