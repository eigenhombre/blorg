(ns blorg.org-spec
  (:require [blorg.org :refer :all]
            [instaparse.core :refer [parses]]
            [speclj.core :refer :all]))


(describe "org-parser"
  (it "parses text with no header lines"
    (should= [[:document [:body "1 2"]]]
             (parses org-parser "1 2")))
  (it "parses text WITH header lines"
    (should= [[:document
               [:hdr "HEAD" "A"]
               [:body "1 2\n"]]]
             (parses org-parser "#+HEAD: A\n1 2\n"))))


(describe "contents->title"
  (it "should get title from contents correctly"
    (should= "My Title"
             (contents->title "#+TITLE: My Title\nStuff\n")))
  (it "shouldn't get a title from something that doesn't have one"
    (should= nil (contents->title "Stuff\n"))))


(describe "contents->headers"
  (it "should get headers correctly"
    (should= {:title "My Title"
              :meta "My Meta"
              :body "Stuff\nMore Stuff"}
             (contents->headers
               "#+TITLE: My Title\n#+META: My Meta\nStuff\nMore Stuff"))))
