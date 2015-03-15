(ns blorg.core
  (:require [blorg.util :refer [pluralize]]
            [blorg.watcher :refer [start-watcher]]
            [clojure.java.shell :refer [sh]]))


(defn announce-file-changes [files]
  (->> files
       count
       (#(format "say %d %s changed" % (pluralize (count files) "file")))
       (#(clojure.string/split % #" "))
       (apply sh))
  files)


(defn display-file-changes [files]
  (printf "%n%d file%s changed:%n"
          (count files)
          (if (= (count files) 1) "" "s"))
  (let [max-print 5
        filenames (map #(.getAbsolutePath %) files)
        print-files (if (< max-print (count files))
                      (-> (take max-print filenames)
                          vec
                          (conj "..."))
                      filenames)]
    (doseq [f print-files]
      (println f))
    files))


(defn handle-changed-files [files]
  (doseq [f files]
    (println f (count (slurp f))))
  files)


(defn watch-directories []
  (start-watcher ["/Users/jacobsen/Dropbox/org"
                  "/Users/jacobsen/Programming/Lisp/Clojure/blorg"]
                 (comp announce-file-changes
                       display-file-changes
                       handle-changed-files)))



(defn wait-forever [] (while true (Thread/sleep 1000)))


(defn -main [& _]
  (watch-directories)
  (wait-forever))


;; REPL conveniences:


(defonce watch-atom (atom nil))


(defn watcher-on []
  (when-not @watch-atom
    (reset! watch-atom (watch-directories))))

(defn watcher-off []
  (when @watch-atom
    (future-cancel @watch-atom)
    (reset! watch-atom nil)))


(comment

(watcher-on)

(watcher-off)

)
