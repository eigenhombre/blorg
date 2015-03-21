(ns blorg.org-spec
  (:require [blorg.org :refer :all]
            [instaparse.core :refer [parses]]
            [speclj.core :refer :all]))


(defn tparse [txt] (->> txt (parses org-parser) txform))


(describe "org-parser"
  (it "parses text with no header lines"
    (should= [[:document [:body "1 2\n"]]]
             (tparse "1 2\n")))
  (it "parses text WITH header lines"
    (should= [[:document
               [:hdr "HEAD" "A"]
               [:body "1 2\n"]]]
             (tparse "#+HEAD: A\n1 2\n")))
  (it "correctly parses text with empty header lines"
    (should= [[:document
               [:hdr "HEAD" "A"]
               [:hdr "EMPTY" ""]
               [:hdr "TAGS" "A B"]
               [:body "1 2\n"]]]
             (tparse "#+HEAD: A\n#+EMPTY:\n#+TAGS: A B\n1 2\n")))
  (it "correctly parses body with a section"
    (should= [[:document
               [:hdr "HEAD" "A"]
               [:section
                [:section-header "*" "Section 1"]
                [:body "Stuff\n"]]]]
             (tparse "#+HEAD: A\n* Section 1\nStuff\n")))
  (it "correctly parses body with TWO sections"
    (should= [[:document
               [:hdr "HEAD" "A"]
               [:section
                [:section-header "*" "Section 1"]]
               [:section
                [:section-header "*" "Section 2"]
                [:body "Stuff\n"]]]]
             (tparse (str "#+HEAD: A\n"
                          "* Section 1\n"
                          "* Section 2\n"
                          "Stuff\n"))))
  (it "correctly parses TWO sections, both with content"
    (should= [[:document
               [:hdr "HEAD" "A"]
               [:section
                [:section-header "*" "Section 1"]
                [:body "Body 1\n"]]
               [:section
                [:section-header "*" "Section 2"]
                [:body "Body 2\n"]]]]
             (tparse (str "#+HEAD: A\n"
                          "* Section 1\n"
                          "Body 1\n"
                          "* Section 2\n"
                          "Body 2\n"))))
  (it (str "correctly parses body with TWO sections, both with content, "
           "where a * is in the first body.")
    (should= [[:document
               [:hdr "HEAD" "A"]
               [:section
                [:section-header "*" "Section 1"]
                [:body "Body * 1\n"]]
               [:section
                [:section-header "*" "Section 2"]
                [:body "Body 2\n"]]]]
             (tparse (str "#+HEAD: A\n"
                          "* Section 1\n"
                          "Body * 1\n"
                          "* Section 2\n"
                          "Body 2\n")))))


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
              :body "Stuff\nMore Stuff\n"}
             (contents->headers
              "#+TITLE: My Title\n#+META: My Meta\nStuff\nMore Stuff\n")))
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


