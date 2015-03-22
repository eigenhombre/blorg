(ns blorg.org-spec
  (:require [blorg.org :refer :all]
            [instaparse.core :refer [parses parser]]
            [speclj.core :refer :all]))


(defn tparse [& txt] (->> txt
                          (apply str)
                          (parses org-parser)))


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
  (it "needs a space after a star to call something a section"
    (should= [[:document
               [:hdr "X" "Y"]
               [:body "*not a section*\n"]]]
             (tparse "#+X: Y\n*not a section*\n")))
  (it "can handle double newlines in body"
    (should= [[:document
               [:body "part a\n" "\n" "part b\n"]]]
             (tparse "part a\n\npart b\n")))
  (it "correctly parses body with TWO sections"
    (should= [[:document
               [:hdr "HEAD" "A"]
               [:section
                [:section-header "*" "Section 1"]]
               [:section
                [:section-header "*" "Section 2"]
                [:body "Stuff\n"]]]]
             (tparse "#+HEAD: A\n"
                     "* Section 1\n"
                     "* Section 2\n"
                     "Stuff\n")))
  (it "correctly parses TWO sections, both with content"
    (should= [[:document
               [:hdr "HEAD" "A"]
               [:section
                [:section-header "*" "Section 1"]
                [:body "Body 1\n"]]
               [:section
                [:section-header "*" "Section 2"]
                [:body "Body 2\n"]]]]
             (tparse "#+HEAD: A\n"
                     "* Section 1\n"
                     "Body 1\n"
                     "* Section 2\n"
                     "Body 2\n")))
  (it "allows header lines between sections"
    (should= [[:document
               [:hdr "TITLE" "X"]
               [:section [:section-header "*" "Section 1"]]
               [:hdr "TOC" "headlines 2"]
               [:section [:section-header "*" "Section 2"]]]]
             (tparse "#+TITLE: X\n"
                     "* Section 1\n"
                     "#+TOC: headlines 2\n"
                     "* Section 2\n")))
  (it "allows header lines between non-section body bits"
    (should= [[:document
               [:hdr "TITLE" "X"]
               [:body "Body 1\n"]
               [:hdr "TOC" "headlines 2"]
               [:body "Body 2\n"]]]
             (tparse "#+TITLE: X\n"
                     "Body 1\n"
                     "#+TOC: headlines 2\n"
                     "Body 2\n")))
  (it "allows and ignores comments in body"
    (should= [[:document
               [:section
                [:section-header "*" "Section"]
                [:body "Body 1\n"]]
               [:body "Body 2\n"]]]
             (tparse "* Section\n"
                     "Body 1\n"
                     "# a comment\n"
                     "Body 2\n")))
  (it "correctly parse when * is in a body"
    (should= [[:document
               [:hdr "HEAD" "A"]
               [:section
                [:section-header "*" "Section 1"]
                [:body "Body * 1\n"]]
               [:section
                [:section-header "*" "Section 2"]
                [:body "Body 2\n"]]]]
             (tparse "#+HEAD: A\n"
                     "* Section 1\n"
                     "Body * 1\n"
                     "* Section 2\n"
                     "Body 2\n")))
  (it "handles level-2 headers"
    (should= [[:document
               [:section
                [:section-header "*" "Section 1"]]
               [:section
                [:section-header "*" "*" "Section 2"]
                [:body "The body.\n"]]]]
             (tparse "* Section 1\n"
                     "** Section 2\n"
                     "The body.\n"))))


(defn pparse [& txt] (->> txt
                          (apply str)
                          (parses paragraph-parser)))


(describe "paragraph parsing"
  (it "handles a newline by itself"
    (should= [[:div]] (pparse "\n")))
  (it "handles a newline before and after a paragraph"
    (should= [[:div
               [:p "something\n"]]]
             (pparse "\nsomething\n")))
  (it "handles two newlines by themselves"
    (should= [[:div]]
             (pparse "\n\n")))
  (it "handles two trailing newlines"
    (should= [[:div
               [:p "something\n"]]]
             (pparse "something\n\n")))
  (it "parses a one-line paragraph"
    (should= [[:div
               [:p "some text"]]]
             (pparse "some text")))
  (it "parses a two-line paragraph"
    (should= [[:div
               [:p "some text\nand more text"]]]
             (pparse "some text\n"
                     "and more text")))
  (it "parses two paragraphs"
    (should= [[:div
               [:p "paragraph one\n"]
               [:p "paragraph two\n"]]]
             (pparse "paragraph one\n\n"
                     "paragraph two\n")))
  (it "parses two paragraphs separated w/ several newlines"
    (should= [[:div
               [:p "paragraph one\n"]
               [:p "paragraph two\n"]]]
             (pparse "paragraph one\n\n\n\n\n"
                     "paragraph two\n"))))


(def body-parser
  (parser "<body> = (default | link)*
           <default> = #'((?!\\[\\[).)*'
           link = <'[['> #'[^\\]]+' <']['> #'[^\\]]+' <']]'>"))


(defn bparse [& txt] (->> txt
                          (apply str)
                          (parses body-parser)))

(describe "Link parsing"
  (it "parses a link"
    (should= [[[:link "a" "b"]]]
             (bparse "[[a][b]]")))
  (it "parses a link with a newline"
    (should= [[[:link "a" "b\nc"]]]
             (bparse "[[a][b\nc]]")))
  (it "handles something extra before a link"
    (should= [["x "
               [:link "a" "b"]]]
             (bparse "x [[a][b]]")))
  (it "handles something extra after a link"
    (should= [[[:link "a" "b"]
               " x"]]
             (bparse "[[a][b]] x"))))
