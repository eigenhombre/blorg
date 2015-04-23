(ns blorg.org
  (:require [blorg.util :refer [vec* selective-walk]]
            [clojure.walk]
            [clojure.zip :as zip]
            [hiccup.util :refer [escape-html]]))


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
  (-> txt
      (clojure.string/replace #"#\+(?:HTML|ATTR_HTML):.+?\n" "")
      (clojure.string/replace #"(?sx)
                                \#\+BEGIN_HTML\s*
                                .*?
                                \#\+END_HTML\s*" "")))


(defn ^:private descend? [el]
  (and (coll? el)
       (not (string? el))
       (not (map? el))
       (not= :pre (first el))))


(defn apply-fn-to-strings
  "
  Walk tree, applying f to each string.  If multiple terms result, put
  them inside a :span tag.
  "
  [f tree]
  (let [f (fn [el]
            (let [[r0 & rs :as r] (f el)]
              (cond
                (string? r) r
                rs (vec* :span r)
                :else r0)))]
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
                       (?!\s+)
                       (.+?)
                       (?!\s+)
                       \+
                     )
                     .
                   )+
                 )?
                 (?:
                   \+
                   (?!\s+)
                   (.+?)
                   (?!\s+)
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


(defn dashify [txt]
  (-> txt
      (clojure.string/replace #"---" "&#x2014;")
      (clojure.string/replace #"--" "&#x2013;")))


(defn get-plain-lists
  "
  Get plain lists and surrounding content out of txt.  Defer actual
  parsing of plain lists.
  "
  [txt]
  (->> txt
       (re-seq #"(?xs)
                 (
                  (?:
                   (?!
                    (?<=\n|^)
                    \ *
                    -
                    \ +
                    [^\n]
                    +\n
                    (?:
                     (?<=\n)
                     (?:\ *-\ +|\ +)
                     [^\n]+
                     \n
                    )*
                   )
                   .
                  )+
                 )?
                 (
                  (?<=\n|^)
                  \ *
                  -
                  \ +
                  [^\n]+
                  \n
                  (?:
                   (?<=\n)
                   (?:\ *-\ +|\ +)
                   [^\n]+
                   \n
                  )*
                 )?")
       (map rest)
       (remove (partial every? empty?))))


(defn items-seq-to-tree
  "
  Convert seq of [level, content] pairs into a tree using zippers.
  Assumes deltas are a whole multiple of two for now.
  "
  [s]
  (loop [[[level x] & more] s
         prev 0
         ret (-> [:ul] zip/vector-zip zip/down)]
    (if-not x
      (zip/root ret)  ;; We're done.
      ;; ... otherwise, figure out where in tree to insert node:
      (recur more level
             (let [delta (/ (- prev level) 2)]
               (cond
                 (> level prev) (-> ret
                                (zip/insert-right [:ul])
                                zip/right
                                zip/down
                                (zip/insert-right [:li x])
                                zip/right)
                 (< level prev) (-> ret
                                (#(last (take (inc delta)
                                              (iterate zip/up %))))
                                (zip/insert-right [:li x])
                                zip/right)
                 :else ;; Simple case -- same level:
                 (-> ret
                     (zip/insert-right [:li x])
                     zip/right)))))))


(defn strip-leading-spaces
  "
  Strip leading spaces from every line in input.
  "
  [txt]
  (let [txt-lines (clojure.string/split txt #"\n")
        spaces-to-strip (->> txt-lines
                             (map (partial re-find #"^( *)"))
                             (map (comp count second))
                             (apply min))]
    (apply str
           (interleave
            (map (comp (partial apply str)
                       (partial drop spaces-to-strip))
                 txt-lines)
            (repeat \newline)))))


(defn parse-plain-list [txt]
  (->> txt
       strip-leading-spaces
       (re-seq #"(?xs)
                 (?<=\n|^)
                 (\ *)-\ +
                 (
                   (?:
                     (?!(?<=\n|^)\ *-\ )
                     .
                   )+
                 )")
       (map rest)
       (map (juxt (comp count first) second))
       items-seq-to-tree))


(defn plain-listify [txt]
  (->> txt
       get-plain-lists
       (mapcat (fn [[before-txt list-txt]]
                 (cond
                   (not before-txt) [(parse-plain-list list-txt)]
                   (not list-txt) [before-txt]
                   :else [before-txt (parse-plain-list list-txt)])))))


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
(defn tree-dashify [tree] (apply-fn-to-strings dashify tree))
(defn tree-listify [tree] (apply-fn-to-strings plain-listify tree))
