(ns blorg.core
  (:require [blorg.mac :refer [say]]
            [blorg.org :refer :all]
            [blorg.util :refer :all]
            [blorg.watcher :refer [start-watcher]]
            [clj-time.core :refer [now]]
            [clojure.java.io :as io]
            [clojure.pprint]
            [environ.core :refer [env]]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [html5 include-css include-js]]
            [hiccup.util :refer [escape-html]]
            [garden.core :as g]))


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


(def all-blog-posts (partial all-org-files date-re))


(defn make-links []
  (html
   [:ul {:class "list-group"}
    (for [f (all-blog-posts)
          :let [link (-> f target-file-name stripdir)
                slurped (slurp f)
                parsed (org-parser slurped)
                extracted-title (doc-title parsed)
                is-draft? (doc-draft parsed)
                tags (doc-tags parsed)
                date-str (date-str-from-file f)
                title (if extracted-title
                        extracted-title
                        (stripdir f))]
          :when (not is-draft?)]
      [:li {:class "list-group-item"}
       [:a {:href link} title]
       ;; FIXME: put in style sheet:
       [:span {:style "padding-left: 10px;"} date-str]
       [:span {:style (str "padding-left: 30px;"
                           "color: #999;"
                           "font-style: italic;")} tags]])]))


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
     [:a {:class "navbar-brand", :href "index.html"} "johnj.com"]]
    [:div
     {:class "collapse navbar-collapse",
      :id "bs-example-navbar-collapse-1"}
     [:ul
      {:class "nav navbar-nav"}
      [:li
       {:class "active"}
       [:a
        {:href "index.html"}
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
      [:li {} [:a {:href "#"} "About"]]]]]])


(defn footer []
  [:div {:class "footer"}
   [:p (format "© %s John Jacobsen."
               (.getYear (now)))]
   [:p (format "Made with %s."
               (html [:a {:href "https://github.com/eigenhombre/blorg"}
                      "blorg"]))]])


(defn css []
  (g/css [:div.footer {:text-align "center"
                       :font-size "13px"
                       :font-style "italic"
                       :color "#666"}]
         [:div.indent {:margin-left "5em"}]
         [:h1.title {:font-size "33px"}]
         [:h1 {:font-size "24px"}]
         [:h2 {:font-size "20px"}]))


(defn allbodies [raw-text]
  (let [first-parse (org-parser raw-text)
        into-paragraphs (as-hiccup first-parse)
        with-markup (xform-paragraphs into-paragraphs)
        with-links (xform-links with-markup)]
    {:raw-text raw-text
     :first-parse first-parse
     :into-paragraphs into-paragraphs
     :with-markup with-markup
     :with-links with-links}))


(defn pre-ify [desc x]
  [:div
   [:a {:id (str "showhide-" desc)
        :href "javascript:void(0)"} desc]
   [:div {:id desc
          :style "display:none;"}
    [:pre
     (-> x
         clojure.pprint/pprint
         with-out-str
         escape-html)]]
   [:script (format (str "$(\"#%s\").click(function()"
                         "  { $(\"#%s\").toggle();"
                         "});")
                    (str "showhide-" desc)
                    desc)]])


(defn head [title]
  [:head
   [:title title]
   (include-css (str "https://maxcdn.bootstrapcdn.com/bootstrap/"
                     "3.3.4/css/bootstrap.min.css"))
   (include-js (str "https://ajax.googleapis.com/ajax/libs/jquery/"
                    "1.11.2/jquery.min.js"))
   (include-js (str "https://maxcdn.bootstrapcdn.com/bootstrap/"
                    "3.3.4/js/bootstrap.min.js"))
   [:style (css)]])


(defn prepare-html [f is-index?]
  (let [slurped (slurp f)
        {:keys [raw-text
                first-parse
                into-paragraphs
                with-markup
                with-links]} (allbodies slurped)
        title (doc-title first-parse)]
    (html5
     {:lang "en"}
     [:meta {:charset "utf-8"}]
     [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
     [:meta {:http-equiv "X-Clacks-Overhead" :content "GNU Terry Pratchett"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
     (head title)
     [:body
      (navbar)
      [:div {:class "container"}
       (let [tags (doc-tags first-parse)
             split-tags (when tags (clojure.string/split tags #" "))
             allbodies [:div
                        with-links
                        [:hr]
                        [:div {:class "indent"}
                         [:p [:em "Intermediate Parses:"]]
                         [:ul
                          [:li (pre-ify "raw-text" raw-text)]
                          [:li (pre-ify "first-parse" first-parse)]
                          [:li (pre-ify "into-paragraphs" into-paragraphs)]
                          [:li (pre-ify "with-markup" with-markup)]
                          [:li (pre-ify "with-links" with-links)]]]]
             date-str (date-str-from-file f)]
         [:div
          ;; FIXME: put date style in style sheet
          [:h1 {:class "title"}
           title [:span {:style (str "font-size:15px;"
                                     "font-style:italic;"
                                     "color:#888;"
                                     "padding-left:20px;")} date-str]]
          (when split-tags
            [:p "Tags: " (for [t split-tags]
                           [:button {:class "btn btn-default btn-xs"} t])])
          (if-not is-index?
            allbodies
            [:div
             allbodies
             (make-links)])])]
      (footer)])))



(defn handle-changed-files [files]
  (doseq [f (-> files
                (conj (str blog-dir "/index.org"))
                set)]
    (future
      (try
        (let [html-name (target-file-name f)
              is-index? (->> f io/file .getName (= "index.org"))
              output-contents (prepare-html f is-index?)]
          (spit html-name output-contents)
          (println (format "Done with %s (%d bytes)"
                           html-name
                           (count output-contents))))
        (catch Throwable t
          (printf "ERROR %s: %s%n" f t)
          (flush)))))
  files)


(defn watch-directories []
  (start-watcher [blog-dir]
                 (comp announce-file-changes
                       display-file-changes
                       handle-changed-files
                       (partial map #(.getAbsolutePath %)))))


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
