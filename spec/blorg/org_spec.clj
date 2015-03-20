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
             (parses org-parser "#+HEAD: A\n1 2\n")))
  (it "correctly parses text with empty header lines"
    (should= [[:document
               [:hdr "HEAD" "A"]
               [:hdr "EMPTY" ""]
               [:hdr "TAGS" "A B"]
               [:body "1 2\n"]]]
             (parses org-parser "#+HEAD: A\n#+EMPTY:\n#+TAGS: A B\n1 2\n"))))


(describe "contents->title"
  (it "gets title from contents correctly"
    (should= "My Title"
             (contents->title "#+TITLE: My Title\nStuff\n")))
  (it "doesn't get a title from something that doesn't have one"
    (should= nil (contents->title "Stuff\n"))))


(describe "contents->headers"
  (it "gets headers correctly"
    (should= {:title "My Title"
              :meta "My Meta"
              :body "Stuff\nMore Stuff"}
             (contents->headers
              "#+TITLE: My Title\n#+META: My Meta\nStuff\nMore Stuff")))
  (it "doesn't correctly parse data after empty header (regression test)"
    (should= {:tags "B C"
              :meta ""
              :title "A"
              :body "Body\n"}
             (contents->headers
              "#+TITLE: A\n#+META:\n#+TAGS: B C\nBody\n")))
  (it "handles (ignores) gaps (newlines) in the hdr block"
    (should= {:title "A"
              :tags "B C"
              :body "Body\n"}
             (contents->headers
              "#+TITLE: A\n\n#+TAGS: B C\nBody\n")))
  (it "ignores LaTeX_HEADER headers"
    (should= {:title "A"
              :body "Body\n"}
             (contents->headers
              "#+TITLE: A\n#+LaTeX_HEADER:blahblah\nBody\n")))
  (it "ignores Org Mode comments ('# stuff...')"
    (should= {:title "A"
              :tags "B C"
              :body "Body\n"}
             (contents->headers
              (str "#+TITLE: A\n\n"
                   "# a comment\n"
                   "#+TAGS: B C\n"
                   "# another comment\n"
                   "Body\n")))))


