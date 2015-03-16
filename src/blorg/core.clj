(ns blorg.core
  (:require [blorg.util :refer [pluralize]]
            [blorg.watcher :refer [start-watcher]]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [hiccup.core :refer [html]]))


(defn say [s]
  (->> s
       (format "say %s")
       (#(clojure.string/split % #" "))
       (apply sh)))


(defn announce-file-changes [files]
  (future
    (let [n (count files)]
      (say (format "%d %s changed" n (pluralize n "file")))))
  files)


(defn display-file-changes [files]
  (printf "%n%d file%s changed:%n"
          (count files)
          (if (= (count files) 1) "" "s"))
  (let [max-print 5
        filenames (map  #(if (string? %)
                           %
                           (.getAbsolutePath %)) files)
        print-files (if (< max-print (count files))
                      (-> (take max-print filenames)
                          vec
                          (conj "..."))
                      filenames)]
    (doseq [f print-files]
      (println f))
    files))


;; FIXME make configurable
(def output-dir "/tmp/blorg")
(def blog-dir "/Users/jacobsen/Dropbox/org/blog/src")


(defn- stripext [s]
  (.substring s 0 (.lastIndexOf s ".")))


(defn- stripdir [s]
  (.substring s
              (inc (.lastIndexOf s "/"))
              (count s)))


(defn- target-file-name [fname]
  (str output-dir "/" (stripext (.getName (io/file fname))) ".html"))


(defn pre-ify [s] (format "<pre>%s</pre>" s))


(defn all-org-files [& [filter-name-regex]]
  (->> blog-dir
       io/file
       .listFiles
       (remove #(.startsWith (.getName %) "."))
       (map #(.getAbsolutePath %))
       (filter #(if filter-name-regex
                  (re-find filter-name-regex %)
                  true))
       (filter #(.endsWith % ".org"))
       reverse))


(def all-blog-posts (partial all-org-files #"\d{4}-\d\d-\d\d-"))


(defn extract-title-from-contents [s]
  (->> s
       (re-find #"\#\+TITLE: (.+)")
       second))


(defn make-links []
  (html [:ul
         (for [f (all-blog-posts)
               :let [link (-> f target-file-name stripdir)
                     extracted-title (-> f slurp extract-title-from-contents)
                     title (if extracted-title
                             extracted-title
                             (stripdir f))]]
           [:li [:a {:href link} title]])]))


(defn prepare-html [f is-index?]
  (let [contents (-> f slurp pre-ify)]
    (if-not is-index?
      contents
      (str contents (make-links)))))


(defn handle-changed-files [files]
  (try (doseq [f (-> files
                     (conj (str blog-dir "/index.org"))
                     set)]
         (let [html-name (target-file-name f)
               is-index? (->> f io/file .getName (= "index.org"))
               output-contents (prepare-html f is-index?)]
           (println (if is-index? (str "**** " f) f) "->" html-name)
           (spit html-name output-contents)))
       (catch Throwable t
         (println t)))
  files)


(defn watch-directories []
  (start-watcher [blog-dir]
                 (comp announce-file-changes
                       display-file-changes
                       handle-changed-files)))


(defn wait-forever []
  (while true (Thread/sleep 1000)))


(defn -main [& _]
  (-> output-dir
      io/file
      .mkdir)
  (watch-directories)
  (wait-forever))


;;; REMOVE BEFORE LEIN OR JAR:
;; (-> (all-org-files)
;;     display-file-changes
;;     handle-changed-files)
;; (spit "/tmp/blorg/links.html" (make-links))
