(ns blorg.core
  (:require [blorg.mac :refer [say]]
            [blorg.org :refer :all]
            [blorg.util :refer :all]
            [blorg.watcher :refer [start-watcher]]
            [clj-time.core :refer [now]]
            [clojure.java.io :as io]
            [clojure.pprint]
            [environ.core :refer [env]]
            [garden.core :as g]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [html5 include-css include-js]]
            [hiccup.util :refer [escape-html]]))


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
(def output-img-dir (str output-dir "/images"))
(def output-html-dir (str output-dir "/html"))
(def blog-dir (-> :home env (str "/Dropbox/org/blog")))
(def blog-src-dir (str blog-dir "/src"))
(def blog-img-dir (str blog-dir "/images"))


(defn- target-html-file-name [fname]
  (str output-html-dir "/" (stripext (.getName (io/file fname))) ".html"))


(defn- target-image-file-name [fname]
  (str output-img-dir "/" (.getName (io/file fname))))


(defn all-img-files []
  (->> blog-img-dir
       io/file
       .listFiles
       (remove #(.startsWith (.getName %) "."))
       (map #(.getAbsolutePath %))))


(defn all-org-files [& [filter-name-regex]]
  (->> blog-src-dir
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
          :let [link (-> f target-html-file-name stripdir)
                slurped (slurp f)
                extracted-title (get-title slurped)
                is-draft? (get-draft slurped)
                tags (get-tags slurped)
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
       {:class "dropdown"}
       [:a
        {:href "#",
         :class "dropdown-toggle",
         :data-toggle "dropdown",
         :role "button",
         :aria-expanded "false"}
        "Topics"
        [:span {:class "caret"}]]
       [:ul
        {:class "dropdown-menu", :role "menu"}
        [:li [:a {:href "#"} "Clojure"]]
        [:li [:a {:href "#"} "Lisp"]]
        [:li [:a {:href "#"} "Physics"]]
        [:li [:a {:href "#"} "Python"]]
        [:li {:class "divider"}]
        [:li [:a {:href "#"} "Art"]]
        [:li {:class "divider"}]
        [:li [:a {:href "#"} "South Pole"]]
        [:li {:class "divider"}]
        [:li [:a {:href "#"} "Other Writing"]]]]]
     [:ul
      {:class "nav navbar-nav"}
      [:li
       {:class "dropdown"}
       [:a
        {:href "#",
         :class "dropdown-toggle",
         :data-toggle "dropdown",
         :role "button",
         :aria-expanded "false"}
        "Contact"
        [:span {:class "caret"}]]
       [:ul
        {:class "dropdown-menu", :role "menu"}
        [:li [:a {:href "https://github.com/eigenhombre"} "GitHub"]]
        [:li [:a {:href "http://twitter.com/eigenhombre"} "Twitter"]]
        [:li [:a {:href (str "http://pgp.mit.edu/pks/lookup?op=vindex&"
                             "search=0x244DD67CD7276AB4")} "GPG Key"]]
        [:li [:a {:href "mailto:eigenhombre@gmail.com"} "Email"]]]]]
     [:ul
      {:class "nav navbar-nav"}
      [:li [:a {:href "#"} "RSS"]]]
     [:ul
      {:class "nav navbar-nav navbar-right"}
      [:li [:a {:href "about.html"} "About"]]]]]])


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
         [:code {:color "#6B464F"
                 :background-color "#FCFCFC"}]
         [:pre.lang_bash {:background-color "#FFFCFA"}]
         [:pre.lang_clojure {:background-color "#F9FFFE"}]
         [:pre.lang_python {:background-color "#FFFAFA"}]
         [:body {:font-size "16px"}]
         [:img.caption {:max-width "100%"}]
         [:.container {:max-width "800px"
                       :margin-left "11.5rem"}]
         [:div.indent {:margin-left "5em"}]
         [:h1.title {:font-size "33px"}]
         [:h1 {:font-size "24px"}]
         [:h2 {:font-size "20px"}]
         [:span.date {:font-size "15px"
                      :font-style "italic"
                      :color "#888"
                      :padding-left "2em"}]))


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


(defn pagination [f]
  (let [posts (remove (comp get-draft slurp) (all-blog-posts))
        ind (.indexOf posts f)
        prev (if (> ind 0) (nth posts (dec ind)))
        next (if (< (inc ind) (dec (count posts))) (nth posts (inc ind)))]
    [:nav
     [:ul {:class "pagination"}
      (if prev
        [:li [:a {:href (-> prev target-html-file-name stripdir)} "Prev"]]
        [:li {:class "disabled"} [:a {:href "#"} "Prev"]])
        [:li [:a {:href "index.html"} "Home"]]
      (if next
        [:li [:a {:href (-> next target-html-file-name stripdir)} "Next"]]
        [:li {:class "disabled"} [:a {:href "#"} "Next"]])]]))


(defn ^:private as-lines [txt]
  (clojure.string/split txt #"\n"))


(defmacro intermediate-parses [& exprs]
  [:div {:class "indent"}
   [:p [:em "Intermediate Parses:"]]
   (vec* :ul
         (for [e exprs]
           `[:li (pre-ify ~(str e) ~e)]))])


(defn prepare-html [f is-index?]
  (let [slurped (slurp f)
        title (get-title slurped)
        [hdrs body] (split-headers-and-body slurped)
        slurped-lines (-> slurped escape-html as-lines)
        display-content (-> body
                            strip-raw-html-tags-for-now
                            convert-body-to-sections
                            tree-srcify
                            tree-example-ify
                            tree-pars
                            tree-linkify
                            tree-captionify
                            tree-boldify
                            tree-emify
                            tree-code-ify
                            tree-hr-ify)]
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
       (pagination f)
       (let [tags (get-tags slurped)
             split-tags (when tags (clojure.string/split tags #" "))
             allbodies [:div
                        display-content
                        (pagination f)
                        #_(intermediate-parses slurped-lines
                                             ppp-content
                                             display-content)]
             date-str (date-str-from-file f)]
         [:div
          [:h1 {:class "title"} title [:span {:class "date"} date-str]]
          (when split-tags
            [:p "Tags: "
             (for [t split-tags]
               [:button {:class "btn btn-default btn-xs"} t])])
          (if-not is-index?
            allbodies
            [:div allbodies (make-links)])])
       (footer)]])))


(defn handle-changed-files [files]
  (doseq [f (-> (filter #(.endsWith % ".org") files)
                (conj (str blog-src-dir "/index.org"))
                set)]
    (future
      (try
        (let [html-name (target-html-file-name f)
              is-index? (->> f io/file .getName (= "index.org"))
              output-contents (prepare-html f is-index?)]
          (spit html-name output-contents)
          (println (format "Done with %s" html-name)))
        (catch Throwable t
          (printf "ERROR %s: %s%n" f t)
          (flush)))))
  (doseq [f (filter is-image-file files)]
    (let [target (io/file (target-image-file-name f))]
      (when-not (.exists target)
        (println (format "COPY IMG %s -> %s\n" f (.getName target)))
        (io/copy (io/file f) target))))
  files)


(defn watch-directories []
  (start-watcher [blog-src-dir blog-img-dir]
                 (comp announce-file-changes
                       display-file-changes
                       handle-changed-files
                       (partial map #(.getAbsolutePath %)))))


(defn -main [& _]
  (doseq [d [output-dir output-img-dir output-html-dir]]
    (-> d io/file .mkdir))
  (watch-directories)
  (wait-forever))


;; ;; ;;; REMOVE BEFORE LEIN OR JAR:
(doseq [d [output-dir output-img-dir output-html-dir]]
  (-> d io/file .mkdir))
(->> (concat (all-org-files) (all-img-files))
     display-file-changes
     handle-changed-files)
:ok
