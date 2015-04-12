(ns blorg.import
  (:require [clojure.java.io :refer [file copy]]
            [blorg.util :refer :all]
            [clj-time.core :as tc]
            [clj-time.coerce :as tcoerce]
            [clj-time.format :as tfmt]))


(defn org-files-in-directory [dirname]
  (->> dirname
       file
       file-seq
       (map str)
       (remove #(.startsWith % "."))
       (filter #(.endsWith % ".org"))
       (map stripdir)
       set))


;; Database dump:
(defn johnj-posts []
  (->> "/Users/jacobsen/Programming/Lisp/clojure/portblog/resources/dump.edn"
       slurp
       read-string
       :navposts
       (filter (comp (partial = 1) :site_id))))


(def date-fmt (tfmt/formatter "yyyy-MM-dd"))

(defn johnj-pages []
  (->> "/Users/jacobsen/Programming/Lisp/clojure/portblog/resources/dump.edn"
       slurp
       read-string
       :navitems
       (filter (comp (partial = 1) :site_id))))


(map (juxt :title :created :slug) (johnj-pages))

;;=>
[["Ph.D. Thesis" nil "phd"]
 ["Meta" nil "about-this-site"]
 ["Art Calendar Article" nil "art-calendar-article"]
 ["UW Article" nil "uw-article"]
 ["Videos" #inst "2009-05-01T21:29:48.000-00:00" "videos"]
 ["Software_old_hidden" nil "software-old"]
 ["NPX Designs, Inc." nil "npx-designs-inc"]
 ["Bio" nil "bio"]
 ["Exhibitions" nil "exhibitions"]
 ["Publications" nil "publications"]
 ["Figure Drawings" nil "figure-drawings"]
 ["Other Prints and Drawings" nil "other-prints-and-drawings"]
 ["Sketchup" nil "sketchup"]
 ["Ruby Plugins For Sketchup" nil "ruby-plugins-for-sketchup"]
 ["Art Geek" nil "artblog"]
 ["Previous Years" nil "pole-previous"]
 ["The Point" #inst "2009-11-28T01:33:22.000-00:00" "the-point"]
 ["The White Continent" nil "the-white-continent"]
 ["The Great Plains" nil "the-great-plains"]
 ["Planetary Convergence" nil "planetary-convergence"]
 ["Other Paintings" nil "other-paintings"]
 ["Photography" nil "photography"]
 ["Country Night Events" nil "country-night-events"]
 ["Election Night in Grant Park" nil "election-night-in-grant-park"]
 ["Flickr Page" nil "flickr-page"]
 ["Drawing" nil "drawing"]
 ["Drawing Course" nil "drawing-course"]
 ["Painting" nil "painting"]
 ["A page with no location" nil "no-where"]
 ["Home" nil "home"]
 ["More Pictures" nil "more-pix-of-me"]
 ["Contact" nil "contact"]
 ["Art" nil "art"]
 ["Musings" #inst "2009-07-08T07:56:05.000-00:00" "musings"]
 ["Party" #inst "2009-09-05T23:05:40.000-00:00" "party"]
 ["Pix of the Day"
  #inst "2010-12-26T20:37:38.000-00:00"
  "pix-of-the-day"]
 ["South Pole" #inst "2011-01-02T09:28:30.000-00:00" "southpole"]
 ["Twitter" #inst "2011-11-02T07:50:36.000-00:00" "link-home-twitter"]
 ["New Work" #inst "2011-07-16T03:31:32.000-00:00" "new-work"]
 ["Software"
  #inst "2012-11-05T06:34:59.000-00:00"
  "link-home-software"]]


(def parent-pages
  (->> (johnj-pages)
       (mapcat (juxt :id :parent_id))
       (apply hash-map)))

;;=>
{291 62, 65 62, 70 67, 62 61, 273 113, 110 109, 72 71, 69 67, 77 71, 119 61, 293 61, 116 115, 113 111, 241 67, 304 61, 117 115, 108 80, 109 122, 118 111, 122 nil, 61 nil, 111 61, 64 62, 286 67, 66 62, 76 71, 277 nil, 202 61, 68 67, 115 61, 112 111, 78 77, 288 61, 79 61, 126 nil, 73 71, 67 61, 71 61, 80 288, 63 62}


(def page-slugs
  (->> (johnj-pages)
       (mapcat (juxt :id :slug))
       (apply hash-map)))
;;=>
{291 "new-work",
 65 "planetary-convergence",
 70 "flickr-page",
 62 "painting",
 273 "phd",
 110 "npx-designs-inc",
 72 "drawing-course",
 69 "election-night-in-grant-park",
 77 "sketchup",
 119 "contact",
 293 "link-home-twitter",
 116 "art-calendar-article",
 113 "publications",
 241 "the-point",
 304 "link-home-software",
 117 "uw-article",
 108 "videos",
 109 "software-old",
 118 "more-pix-of-me",
 122 "no-where",
 61 "home",
 111 "bio",
 64 "the-great-plains",
 286 "pix-of-the-day",
 66 "other-paintings",
 76 "other-prints-and-drawings",
 277 "party",
 202 "musings",
 68 "country-night-events",
 115 "about-this-site",
 112 "exhibitions",
 78 "ruby-plugins-for-sketchup",
 288 "southpole",
 79 "artblog",
 126 "art",
 73 "figure-drawings",
 67 "photography",
 71 "drawing",
 80 "pole-previous",
 63 "the-white-continent"}


(->> (johnj-posts)
     (map (juxt (comp (partial tfmt/unparse date-fmt)
                      tcoerce/from-date
                      :created)
                (comp page-slugs :page_id)
                :access
                :slug))
     vec)

;;=>
[["2008-11-24" "artblog" "U" "another-work-in-progress"]
 ["2008-11-23" "artblog" "U" "columbia"]
 ["2008-11-25" "artblog" "U" "dreads"]
 ["2008-11-21" "artblog" "U" "fall-2008-hpac-studies"]
 ["2008-11-30"
  "artblog"
  "U"
  "gouache-and-a-new-system-for-conquering-the-world"]
 ["2009-04-06" "artblog" "U" "head"]
 ["2009-04-24" "artblog" "U" "heads"]
 ["2009-04-22" "artblog" "U" "hybrids"]
 ["2009-03-31" "artblog" "U" "imaginary-digital-towns"]
 ["2009-03-02" "artblog" "U" "metric"]
 ["2009-05-12" "artblog" "U" "more-experimental-results"]
 ["2009-04-12" "artblog" "U" "more-posing"]
 ["2009-04-20" "artblog" "U" "people"]
 ["2009-04-05" "artblog" "U" "posing-rigs"]
 ["2008-07-30" "software-old" "U" "oscon-2008"]
 ["2009-04-18" "artblog" "U" "progress"]
 ["2009-04-04" "artblog" "U" "relapse"]
 ["2009-04-14" "artblog" "U" "rendering"]
 ["2009-04-15" "artblog" "U" "strange-blimps"]
 ["2009-02-28" "artblog" "U" "studies"]
 ["2009-05-09" "musings" "U" "where-are-our-alien-friends"]
 ["2009-01-17" "artblog" "U" "wyeth-on-another-planet"]
 ["2008-11-27" "artblog" "U" "yafp-yet-another-flood-painting"]
 ["2008-03-15" "software-old" "U" "pycon-and-django"]
 ["2009-05-18" "software-old" "U" "python-concurrency-workshop"]
 ["2009-02-22" "pole-previous" "U" "cuba"]
 ["2009-02-16" "pole-previous" "U" "wickets"]
 ["2009-07-08" "musings" "U" "cryptonomikindle"]
 ["2009-07-08" "musings" "U" "moving-day"]
 ["2009-07-08" "musings" "U" "curmudgeons-guide-to-health-care"]
 ["2009-02-12" "pole-previous" "U" "broken-glasses"]
 ["2008-02-18" "pole-previous" "U" "a-bath"]
 ["2009-01-17" "pole-previous" "U" "chc"]
 ["2009-01-16" "pole-previous" "U" "detox"]
 ["2009-02-06" "pole-previous" "U" "end-of-the-second-act"]
 ["2009-01-06" "pole-previous" "U" "gearing-up"]
 ["2008-02-16" "pole-previous" "U" "green-marathon"]
 ["2009-01-29" "pole-previous" "U" "immortality"]
 ["2009-01-30" "pole-previous" "U" "in-a-dry-and-waterless-place"]
 ["2009-01-31" "pole-previous" "U" "last-arrivals"]
 ["2009-01-31" "pole-previous" "U" "lily-white"]
 ["2009-01-09" "pole-previous" "U" "nails"]
 ["2009-01-27" "pole-previous" "U" "ok-but-busy"]
 ["2008-02-13" "pole-previous" "U" "outta-here"]
 ["2009-01-14" "pole-previous" "U" "packing"]
 ["2009-01-22" "pole-previous" "U" "passing-notes"]
 ["2009-02-04" "pole-previous" "U" "pigs-and-fish"]
 ["2009-01-19" "pole-previous" "U" "rnzaf"]
 ["2009-01-29" "pole-previous" "U" "routine"]
 ["2009-02-15" "pole-previous" "U" "safe"]
 ["2008-02-15" "pole-previous" "U" "sprung"]
 ["2009-01-19" "pole-previous" "U" "the-usual-delays"]
 ["2009-01-28" "pole-previous" "U" "tourists"]
 ["2009-01-20" "pole-previous" "U" "translation"]
 ["2008-11-20" "pole-previous" "U" "yabp-yet-another-blog-platform"]
 ["2009-10-09" "musings" "U" "man-on-wire"]
 ["2009-10-12" "artblog" "U" "transparency"]
 ["2009-10-03" "artblog" "U" "back-at-it"]
 ["2009-11-05" "artblog" "U" "veneto"]
 ["2009-11-07" "artblog" "U" "otl"]
 ["2009-11-08" "artblog" "U" "collected-planets"]
 ["2009-11-10" "musings" "U" "the-cruel-stranger"]
 ["2009-12-16" "the-point" "U" "entering-winter"]
 ["2010-01-03" "artblog" "I" "practice"]
 ["2010-01-10" "artblog" "U" "sea-and-silence"]
 ["2010-01-11" "artblog" "U" "silhouettes"]
 ["2010-01-23" "artblog" "U" "big-structure-studies"]
 ["2010-01-23" "the-point" "U" "daily-horizon"]
 ["2010-02-11" "artblog" "U" "snow"]
 ["2010-02-11" "artblog" "U" "completion"]
 ["2010-02-12" "artblog" "U" "last-year"]
 ["2010-02-16" "software-old" "I" "passing-the-torch"]
 ["2010-02-17" "artblog" "U" "more-invisible-cities"]
 ["2010-02-18" "musings" "U" "clippings"]
 ["2010-02-21" "artblog" "U" "the-poetry-of-small-things"]
 ["2010-03-07" "artblog" "U" "flags-and-bubbles"]
 ["2010-03-08" "artblog" "U" "volcanic-surfaces-of-revolution"]
 ["2010-03-28" "artblog" "U" "tilting"]
 ["2010-05-15" "artblog" "U" "more-prototyping"]
 ["2010-05-23" "artblog" "U" "actually-carrying-it-out"]
 ["2010-09-13" "pole-previous" "U" "coming-out-of-hibernation"]
 ["2010-05-25" "artblog" "U" "yesterdays-dream"]
 ["2010-09-18" "artblog" "U" "actually-carrying-it-out-ii"]
 ["2010-09-20" "artblog" "U" "making-space"]
 ["2010-10-18" "artblog" "U" "evolution"]
 ["2010-10-18" "artblog" "U" "nother"]
 ["2010-10-19" "artblog" "U" "on-the-drying-of-various-polymers"]
 ["2010-10-27"
  "artblog"
  "U"
  "playing-around-with-golden-open-acrylics"]
 ["2010-11-01" "artblog" "U" "compositing-2d-and-3d"]
 ["2010-11-06" "artblog" "U" "more-problem-solving"]
 ["2010-12-22" "pix-of-the-day" "U" "december-21"]
 ["2010-12-22" "pix-of-the-day" "U" "december-21-again"]
 ["2010-12-16" "the-point" "U" "late-2010"]
 ["2010-12-19" "artblog" "U" "photography"]
 ["2010-12-13" "pole-previous" "U" "downhole"]
 ["2010-12-22" "pix-of-the-day" "U" "december-22"]
 ["2010-12-23" "pix-of-the-day" "U" "december-23"]
 ["2010-12-24" "pix-of-the-day" "U" "december-24"]
 ["2010-12-25" "pix-of-the-day" "U" "december-25"]
 ["2010-12-26" "pix-of-the-day" "U" "december-26"]
 ["2010-12-27" "pix-of-the-day" "U" "december-27"]
 ["2010-12-29" "pix-of-the-day" "U" "december-28"]
 ["2010-12-30" "pix-of-the-day" "U" "december-29"]
 ["2010-12-30" "pix-of-the-day" "U" "december-30"]
 ["2010-12-31" "pix-of-the-day" "U" "december-31----happy-new-year"]
 ["2011-01-02" "southpole" "U" "january-1-2011"]
 ["2011-01-03" "southpole" "U" "january-3-2011"]
 ["2011-01-04" "southpole" "U" "january-4-2011"]
 ["2011-01-05" "southpole" "U" "january-5-2011"]
 ["2011-01-06" "southpole" "U" "january-6-2011"]
 ["2011-01-06" "southpole" "U" "january-7-2011"]
 ["2011-01-08" "southpole" "U" "january-8-2011"]
 ["2011-01-09" "southpole" "U" "january-9-2011"]
 ["2011-01-10" "southpole" "U" "january-10-2011"]
 ["2011-01-11" "southpole" "U" "january-11-2011"]
 ["2011-01-12" "southpole" "U" "january-12-2011"]
 ["2011-01-13" "southpole" "U" "january-13-2011"]
 ["2011-01-14" "southpole" "U" "january-14-2011"]
 ["2011-01-15" "southpole" "U" "january-15-2011"]
 ["2011-01-16" "southpole" "U" "january-16-2011"]
 ["2011-01-17" "southpole" "U" "january-17-2011"]
 ["2011-01-18" "southpole" "U" "january-18-2011"]
 ["2011-01-19" "southpole" "U" "january-19-2011"]
 ["2011-01-20" "southpole" "U" "january-20-2011"]
 ["2011-01-21" "southpole" "U" "january-21-2011"]
 ["2011-01-22" "southpole" "U" "january-22-2011"]
 ["2011-01-23" "southpole" "U" "january-23-2011"]
 ["2011-01-24" "southpole" "U" "january-24-2011"]
 ["2011-01-25" "southpole" "U" "january-25-2011"]
 ["2011-04-12" "artblog" "U" "graphite-transfers"]
 ["2011-04-17" "artblog" "U" "figure-drawing-restart"]
 ["2011-05-29" "musings" "U" "in-defense-of-hobbies"]
 ["2011-06-05" "software-old" "U" "oscon-video"]
 ["2011-06-27" "artblog" "U" "sketchup-and-makehuman"]
 ["2011-06-28" "artblog" "U" "more-space"]
 ["2011-07-29" "southpole" "U" "iso50"]
 ["2011-08-28" "artblog" "U" "plein-air"]
 ["2011-10-19" "artblog" "U" "the-wheel"]
 ["2011-11-02" "southpole" "U" "ten"]
 ["2011-11-08" "southpole" "U" "the-quiet-earth"]
 ["2011-11-09" "southpole" "I" "seals-and-tape"]
 ["2011-11-11" "southpole" "U" "a-nicer-guy"]
 ["2011-11-12" "southpole" "U" "bon-voyage"]
 ["2011-11-13" "southpole" "U" "drifts"]
 ["2011-11-15" "southpole" "U" "reveille"]
 ["2011-11-17" "southpole" "U" "traveller-and-the-human-chain"]
 ["2011-11-20" "southpole" "U" "fresh-air-and-bananas"]
 ["2011-11-23" "southpole" "U" "shower-instructions"]
 ["2011-11-25" "southpole" "U" "wind-storm-and-moon-dust"]
 ["2011-11-27" "southpole" "U" "turkey-stuffing-eclipse"]
 ["2011-11-29" "southpole" "U" "ghost"]
 ["2011-11-29" "southpole" "U" "oxygen"]
 ["2011-12-03" "southpole" "U" "milvans-and-container-malls"]
 ["2011-12-23" "software-old" "U" "programming-languages"]
 ["2012-04-01"
  "software-old"
  "U"
  "continuous-testing-in-python-and-clojure"]
 ["2012-05-21" "software-old" "U" "resources-for-learning-clojure"]]



(defn tags-iterate [[prev-tags id]]
  (when id
    [(conj prev-tags (page-slugs id))
     (parent-pages id)]))


(defn tags-for-page-id [id]
  (->> [[] id]
       (iterate tags-iterate)
       (take-while (complement nil?))
       last
       first
       (remove #{"home"})))


(defn dissoc-unneeded-keys [m]
  (dissoc m :id :updated :site_id :author_id :in_rss :fmt))


(defn tag-post-from-post-page [p]
  (assoc p :tags (-> p :page_id tags-for-page-id vec)))


(defn tag-post-with-joined-str [p]
  (assoc p :tagstr (->> p :tags (clojure.string/join ", "))))


(defn add-file-name [p]
  (assoc p :filename (str (->> p
                               :created
                               tcoerce/from-date
                               (tfmt/unparse date-fmt))
                          "-"
                          (:slug p)
                          "-coriolis.org")))


(defn imagify-pix [s]
  (clojure.string/replace s #"\[\[([^\]]+)\]\]" "[[../images/$1.jpg]]"))


(defn orgify-links [s]
  (clojure.string/replace s #"(?sx)\"([^\"]+)\":(.+?)(?:\.|,)?(?=\s|$|\")"
                          "[[$2][$1]]"))


(defn h1 [s]
  (clojure.string/replace s #"(?<=^|\n)h1\.\s+(.+?)(?=\n|$)" "* $1"))


(defn h2 [s]
  (clojure.string/replace s #"(?<=^|\n)h2\.\s+(.+?)(?=\n|$)" "** $1"))


(defn h3 [s]
  (clojure.string/replace s #"(?<=^|\n)h3\.\s+(.+?)(?=\n|$)" "*** $1"))


(defn fixcr [s]
  (clojure.string/replace s #"\r\n" "\n"))


(defn remove-ps [s]
  (clojure.string/replace s #"(?<=^|\n)p\.\s+(.+?)(?=\n|$)" "$1"))


(defn fix-italics [s]
  (clojure.string/replace s #"__([^_]+)__" "/$1/"))


(defn orgify [p]
  (assoc p :org
         (format "#+TITLE: %s\n#+TAGS: %s\n\n%s\n"
                 (:title p)
                 (:tagstr p)
                 (-> p
                     :body
                     fixcr
                     h1
                     h2
                     h3
                     remove-ps
                     fix-italics
                     imagify-pix
                     orgify-links))))


(defn is-public? [p] (-> p :access (= "U")))


(defn old-software-page? [p] (-> p :page_id (= 109)))


(defn transformed-posts []
  (->> (johnj-posts)
       (map dissoc-unneeded-keys)
       (map tag-post-from-post-page)
       (map tag-post-with-joined-str)
       (map add-file-name)
       (filter is-public?)
       (remove old-software-page?)
       (map orgify)))


(def output-dir "/Users/jacobsen/Dropbox/org/blog/src")


(-> output-dir file .mkdir)
(doseq [m (transformed-posts)]
  (spit (str output-dir "/" (:filename m))
        (:org m)))


;;=>
[{:tags ["artblog"],
  :slug "another-work-in-progress",
  :page_id 79,
  :created #inst "2008-11-24T04:33:27.000-00:00",
  :title "Another work in progress",
  :filename "2008-11-24-another-work-in-progress.org",
  :org
  "#+TITLE: Another work in progress\n#+TAGS:artblog\n\n\n[[../images/IMG_1222]]\n\nHere's another one I've been working on off and on for quite a while, originally based on\n[[../1_Painting/4_Other_Paintings/reaction00_1_full-display.html][Reaction (2000)]]  There was \na floating figure in the sky to the right which really wasn't working so I painted it\nout.  Several more hours to go on this one, I think.  Oil on top of acrylic on panel.\n\n[[../images/IMG_1222_2]]\nDetail\n\nAs you can see from the detail, I transferred a portion of a sketch I did using\nthe traditional method of gridding both the panel and the sketch.\n\n\n\n",
  :access "U",
  :body
  "\n[[IMG_1222]]\n\nHere's another one I've been working on off and on for quite a while, originally based on\n\"Reaction (2000)\":../1_Painting/4_Other_Paintings/reaction00_1_full-display.html.  There was \na floating figure in the sky to the right which really wasn't working so I painted it\nout.  Several more hours to go on this one, I think.  Oil on top of acrylic on panel.\n\n[[IMG_1222_2]]\nDetail\n\nAs you can see from the detail, I transferred a portion of a sketch I did using\nthe traditional method of gridding both the panel and the sketch.\n\n\n\n",
  :tagstr "artblog"}]
