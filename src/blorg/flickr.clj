(ns blorg.flickr
  "
  (Probably temporary) namespace to fetch and save a Flickr photo to disk.

  Once all relevant photos have been pulled down from Flickr, this
  file (and the flickr4java dependency in project.clj) can probably go away.
  "
  (:import [com.flickr4java.flickr Flickr REST]
           [com.flickr4java.flickr.auth AuthInterface]
           [com.flickr4java.flickr.photos Photo PhotosInterface]
           [javax.imageio.ImageIO])
  (:require [environ.core :refer [env]]))


(defonce flickr-api
  (Flickr. (env :flickr-api-key)
           (env :flickr-api-secret)
           (REST.)))


(defn save-flickr-photo-to-dir [flickr-id dir]
  (let [fileob (clojure.java.io/file (format "%s/%s.jpg" dir flickr-id))
        content (.. flickr-api
                    (getPhotosInterface)
                    (getPhoto flickr-id)
                    getOriginalImage)]
    (javax.imageio.ImageIO/write content "jpg" fileob)))
