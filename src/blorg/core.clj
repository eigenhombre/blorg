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


(def output-dir (or (:blorg-home env) "/tmp/blorg"))
(def output-img-dir (str output-dir "/images"))
(def output-assets-dir (str output-dir "/assets"))
(def output-html-dir (str output-dir "/html"))
;; FIXME: make configurable:
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


(defn create-needed-directories []
  (doseq [d [output-dir output-img-dir output-html-dir output-assets-dir]]
    (-> d io/file .mkdir)))


(defn get-asset-list []
  (-> (clojure.java.io/resource "assets")
      io/file
      .listFiles))


(defn copy-assets []
  (doseq [f (get-asset-list)]
    (io/copy f (io/file (str output-assets-dir "/" (.getName f))))))


(def all-blog-posts (partial all-org-files date-re))


(defn make-links []
  (html
   [:h2 "Blog Posts"]
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
      [:li
       [:a {:href link} title]
       [:span {:class "link-date"} date-str]
       [:span {:class "tags"} tags]])]))


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
     [:a {:class "navbar-brand", :href "index.html"}
      [:span {:class "palegrey"} "johnj.com"]]]
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
        [:li [:a {:href "art.html"} "Art"]]
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
      {:class "nav navbar-nav"}
      [:li [:a {:href "about.html"} "About"]]]]]])


(defn footer [postyear]
  [:div {:class "footer"}
   [:p (format "© %s John Jacobsen."
               (let [this-year (.getYear (now))]
                 (if (= (str postyear) (str this-year))
                   this-year
                   (str postyear "-" this-year))))]
   [:p (format "Made with %s."
               (html [:a {:href "https://github.com/eigenhombre/blorg"}
                      "blorg"]))]])


(defn gen-css []
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
         [:.palegrey {:color "rgb(218, 218, 218)"}]
         [:img.caption {:max-width "100%"}]
         [:img.caption {:max-height "600px"}]
         [:.container {:max-width "800px"
                       :margin-left "11.5rem"}]
         [:div.indent {:margin-left "5em"}]
         [:h1.title {:font-size "33px"}]
         [:h1 {:font-size "24px"}]
         [:h2 {:font-size "20px"}]
         [:span.tags {:padding-left "30px"
                      :color "#999"
                      :font-style "italic"}]
         [:span.link-date {:padding-left "10px"
                           :color "#bfa9a9"}]
         [:span.date-in-header {:font-size "15px"
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


(defn get-assets-by-ext [ext]
  (get (->> (get-asset-list)
            (map #(.getName %))
            (group-by fileext))
       (str "." ext)))


(defn head [title]
  (let [css (get-assets-by-ext "css")
        js (get-assets-by-ext "js")
        add-asset-dir (partial str "../assets/")
        includes (concat (mapcat (comp include-css add-asset-dir) css)
                         (mapcat (comp include-js add-asset-dir) js))]
    `[:head
      [:title title]
      ~@includes
      [:style ~(gen-css)]]))


(defn pagination [f]
  (let [posts (remove (comp get-draft slurp) (all-blog-posts))
        ind (.indexOf posts f)
        next (if (> ind 0) (nth posts (dec ind)))
        prev (if (< ind (dec (count posts))) (nth posts (inc ind)))]
    [:nav
     [:ul {:class "pagination"}
      (if prev
        [:li [:a {:href (-> prev target-html-file-name stripdir)} "Past"]]
        [:li {:class "disabled"} [:a {:href "#"} "Past"]])
      [:li [:a {:href "index.html"} "Home"]]
      (if next
        [:li [:a {:href (-> next target-html-file-name stripdir)} "Future"]]
        [:li {:class "disabled"} [:a {:href "#"} "Future"]])]]))


(defn ^:private as-lines [txt]
  (clojure.string/split txt #"\n"))


(defmacro intermediate-parses [& exprs]
  [:div {:class "indent"}
   [:p [:em "Intermediate Parses:"]]
   (vec* :ul
         (for [e exprs]
           `[:li (pre-ify ~(str e) ~e)]))])


(defn disqus []
  [:div [:div {:id "disqus_thread"}]
   [:script {:type "text/javascript"}
    "var disqus_shortname = 'eigenhombrecom';
     (function() {
       var dsq = document.createElement('script');
       dsq.type = 'text/javascript';
       dsq.async = true;
       dsq.src = 'http://' + disqus_shortname + '.disqus.com/embed.js';
       (document.getElementsByTagName('head')[0] ||
        document.getElementsByTagName('body')[0]).appendChild(dsq);
     })();"]
   [:noscript "Please enable JavaScript to view the"
    [:a {:href "http://disqus.com/?ref_noscript"}]
    "comments powered by Disqus."]
   [:a {:href "http://disqus.com" :class "dsq-brlink"}
    "blog comments powered by " [:span {:class "logo-disqus"} "Disqus"]]])


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
                            tree-listify
                            tree-pars
                            tree-linkify
                            tree-captionify
                            tree-boldify
                            tree-emify
                            tree-code-ify
                            tree-strike-ify
                            tree-hr-ify
                            tree-dashify)]
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
       (when-not is-index? (pagination f))
       (let [tags (get-tags slurped)
             split-tags (when tags (clojure.string/split tags #","))
             allbodies [:div
                        display-content
                        (when-not is-index? (pagination f))
                        #_(intermediate-parses slurped-lines
                                             display-content)]
             date-str (date-str-from-file f)]
         [:div
          [:h1 {:class "title"}
           title [:span {:class "date-in-header"} date-str]]
          (when split-tags
            [:p "Tags: "
             (for [t split-tags]
               [:button {:class "btn btn-default btn-xs"} t])])
          (if-not is-index?
            allbodies
            [:div allbodies (make-links)])])
       (footer (year-from-file f))
       (disqus)]])))


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
  (create-needed-directories)
  (copy-assets)
  (watch-directories)
  (wait-forever))


;; ;;; REMOVE BEFORE LEIN OR JAR:
;; (create-needed-directories)
;; (copy-assets)
;; (->> (concat (all-org-files) (all-img-files))
;;      display-file-changes
;;      handle-changed-files)
;; :ok
