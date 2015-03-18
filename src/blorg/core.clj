(ns blorg.core
  (:require [blorg.util :refer [pluralize]]
            [blorg.watcher :refer [start-watcher]]
            [clj-time.coerce :as tc]
            [clj-time.format :as tfmt]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [environ.core :refer [env]]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [html5 include-css include-js]]))


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
(def blog-dir (-> :home env (str "/Dropbox/org/blog/src")))


(defn- stripext [s]
  (.substring s 0 (.lastIndexOf s ".")))


(defn- stripdir [s]
  (.substring s
              (inc (.lastIndexOf s "/"))
              (count s)))


(defn- target-file-name [fname]
  (str output-dir "/" (stripext (.getName (io/file fname))) ".html"))


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


(def ^:private date-re #"\d{4}-\d\d-\d\d")
(def ^:private formatter (tfmt/formatter "yyyy-MM-dd"))
(def all-blog-posts (partial all-org-files date-re))


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


(defn extract-title-from-contents [s]
  (->> s
       (re-find #"\#\+TITLE: (.+)")
       second))


(defn make-links []
  (html
   [:ul
    (for [f (all-blog-posts)
          :let [link (-> f target-file-name stripdir)
                extracted-title (-> f slurp extract-title-from-contents)
                date-str (date-str-from-file f)
                title (if extracted-title
                        extracted-title
                        (stripdir f))]]
      [:li
       [:a {:href link} title]
       [:span {:class "filedate"} date-str]])]))


(defn navbar []
  [:nav
   {:class "navbar navbar-default"}
   [:div
    {:class "container-fluid"}
    [:div
     {:class "navbar-header"}
     [:button
      {:type "button",
       :class "navbar-toggle collapsed",
       :data-toggle "collapse",
       :data-target "#bs-example-navbar-collapse-1"}
      [:span {:class "sr-only"} "Toggle navigation"]
      [:span {:class "icon-bar"}]
      [:span {:class "icon-bar"}]
      [:span {:class "icon-bar"}]]
     [:a {:class "navbar-brand", :href "#"} "johnj.com"]]
    [:div
     {:class "collapse navbar-collapse",
      :id "bs-example-navbar-collapse-1"}
     [:ul
      {:class "nav navbar-nav"}
      [:li
       {:class "active"}
       [:a
        {:href "#"}
        "Home"
        [:span {:class "sr-only"} "(current)"]]]
      [:li
       {:class "dropdown"}
       [:a
        {:href "#",
         :class "dropdown-toggle",
         :data-toggle "dropdown",
         :role "button",
         :aria-expanded "false"}
        "Tags"
        [:span {:class "caret"}]]
       [:ul
        {:class "dropdown-menu", :role "menu"}
        [:li {} [:a {:href "#"} "Lisp"]]
        [:li {} [:a {:href "#"} "Physics"]]
        [:li {} [:a {:href "#"} "Python"]]
        [:li {} [:a {:href "#"} "Clojure"]]
        [:li {:class "divider"}]
        [:li {} [:a {:href "#"} "Art"]]
        [:li {:class "divider"}]
        [:li {} [:a {:href "#"} "Writing"]]]]]
     [:form
      {:class "navbar-form navbar-left", :role "search"}
      [:div
       {:class "form-group"}
       [:input
        {:type "text", :class "form-control", :placeholder "Search"}]]
      [:button {:type "submit", :class "btn btn-default"} "Submit"]]
     [:ul
      {:class "nav navbar-nav navbar-right"}
      [:li {} [:a {:href "#"} "About"]]
      [:li
       {:class "dropdown"}
       [:a
        {:href "#",
         :class "dropdown-toggle",
         :data-toggle "dropdown",
         :role "button",
         :aria-expanded "false"}
        "Tags"
        [:span {:class "caret"}]]
       [:ul
        {:class "dropdown-menu", :role "menu"}
        [:li {} [:a {:href "#"} "Lisp"]]
        [:li {} [:a {:href "#"} "Physics"]]
        [:li {} [:a {:href "#"} "Python"]]
        [:li {} [:a {:href "#"} "Clojure"]]
        [:li {:class "divider"}]
        [:li {} [:a {:href "#"} "Art"]]
        [:li {:class "divider"}]
        [:li {} [:a {:href "#"} "Writing"]]]]]]]])


(defn prepare-html [f is-index?]
  (html5
   {:lang "en"}
   [:meta {:charset "utf-8"}]
   [:meta {:http-equiv "X-UA-Compatible"
           :content "IE=edge"}]
   [:meta {:name "viewport"
           :content "width=device-width, initial-scale=1"}]
   [:head
    (include-css (str "https://maxcdn.bootstrapcdn.com/bootstrap/"
                      "3.3.4/css/bootstrap.min.css"))
    (include-js (str "https://ajax.googleapis.com/ajax/libs/jquery/"
                     "1.11.2/jquery.min.js"))
    (include-js (str "https://maxcdn.bootstrapcdn.com/bootstrap/"
                     "3.3.4/js/bootstrap.min.js"))]
   [:body
    (navbar)
    (let [contents (->> f slurp (vector :pre) html)
          title (-> f slurp extract-title-from-contents)
          date-str (date-str-from-file f)]
      [:div
       [:h1 title]
       [:span date-str]
       (if-not is-index?
         contents
         (str contents (make-links)))])]))


(defn handle-changed-files [files]
  (try (doseq [f (-> files
                     (conj (str blog-dir "/index.org"))
                     set)]
         (let [html-name (target-file-name f)
               is-index? (->> f io/file .getName (= "index.org"))
               output-contents (prepare-html f is-index?)]
           (spit html-name output-contents)))
       (catch Throwable t
         (println t)))
  files)


(defn watch-directories []
  (start-watcher [blog-dir]
                 (comp announce-file-changes
                       display-file-changes
                       handle-changed-files
                       (partial map #(.getAbsolutePath %)))))


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
;;     announce-file-changes
;;     display-file-changes
;;     handle-changed-files)
;; :ok
