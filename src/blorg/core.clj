(ns blorg.core
  (:require [blorg.util :refer [pluralize]]
            [blorg.watcher :refer [start-watcher]]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]))


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


(defn- target-file-name [fname]
  (str output-dir "/" (stripext (.getName (io/file fname))) ".html"))


(defn pre-ify [s] (format "<pre>%s</pre>" s))


(defn handle-changed-files [files]
  (doseq [f files]
    (let [html-name (target-file-name f)
          output-contents (-> f slurp pre-ify)]
      (println f "->" html-name)
      (spit html-name output-contents)))
  files)


(defn all-blog-files []
  (->> blog-dir
       io/file
       .listFiles
       (remove #(.startsWith (.getName %) "."))
       (map #(.getAbsolutePath %))
       (filter #(.endsWith % ".org"))))


(defn watch-directories []
  (start-watcher [blog-dir]
                 (comp announce-file-changes
                       display-file-changes
                       handle-changed-files)))


(defn wait-forever []
  (while true (Thread/sleep 1000)))


;;; REMOVE BEFORE LEIN OR JAR:
;; (-> (all-blog-files)
;;     display-file-changes
;;     handle-changed-files)


(defn -main [& _]
  (-> output-dir
      io/file
      .mkdir)
  (watch-directories)
  (wait-forever))
