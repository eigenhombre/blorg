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


(defn filename->timestr [filename]
  (->> filename
       io/file
       .lastModified
       tc/from-long))


(defn file-date-str [filename]
  {:pre [(string? filename)]}
  (or (filename->timestr filename)
      (filename->timestr filename)))


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
