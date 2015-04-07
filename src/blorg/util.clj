(ns blorg.util
  (:require [clj-time.coerce :as tc]
            [clj-time.format :as tfmt]
            [clojure.java.io :as io]))


(defn pluralize [n s]
  (format "%s%s" s (if (= n 1) "" "s")))


(defn wait-forever []
  (while true (Thread/sleep 1000)))


(def date-re #"\d{4}-\d\d-\d\d")
(def formatter (tfmt/formatter "yyyy-MM-dd"))


(defn filename->timestr [filename]
  (some->> filename
           (re-find date-re)
           (tfmt/parse formatter)))


(defn file-creation-date [filename]
  (->> filename
       io/file
       .lastModified
       tc/from-long))


(defn file-date-str [filename]
  {:pre [(string? filename)]}
  (or (filename->timestr filename)
      (file-creation-date filename)))


(defn date-str-from-file [filename]
  (->> filename
       file-date-str
       (tfmt/unparse formatter)))


(defn stripdir [s]
  (.substring s
              (inc (.lastIndexOf s "/"))
              (count s)))


(defn stripext [s]
  (.substring s 0 (.lastIndexOf s ".")))


(defn fileext [s]
  (let [i (.lastIndexOf s ".")]
    (when (pos? i)
      (.substring s (.lastIndexOf s ".") (count s)))))


(defn is-image-file [f]
  (some->> f
           fileext
           clojure.string/lower-case
           (contains? #{".img" ".png" ".jpg" ".jpeg" ".gif"})))


(defn vec*
  "
  Like list*, but for vectors.  (vec* :a :b [:c :d]) => [:a :b :c :d].
  "
  [& args]
  (let [l (last args)
        bl (butlast args)]
    (vec (concat bl l))))


(defn selective-walk
  "
  Walk tree recursively, descending into subtrees only when descend?
  on the subtree is truthy, and transforming elements only when
  transform? is truthy.
  "
  [action descend? transform? form]
  (let [walk-fn
        (fn [el]
          (cond
           (transform? el) (action el)
           (and (coll? el)
                (descend? el)) (selective-walk action descend? transform? el)
                :else el))]
    (if (sequential? form)  ;; FIXME: generalize to maps like clojure.walk does?
      (into (empty form) (map walk-fn form))
      form)))

