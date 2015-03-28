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


(defmacro describe-examples [right-fn left-fn & body]
  `(describe "Examples"
     ~@(for [[l r] (partition 2 body)]
         `(~'it ~l
            (~'should= (~right-fn ~r) (~left-fn ~l))))))


(describe-examples vector (partial parses body-parser)
  "\n"             [:div]
  "x\n"            [:div [:p "x\n"]]
  "\n\n"           [:div]
  "x\n\n"          [:div [:p "x\n"]]
  "some text"      [:div [:p "some text"]]
  "x\nmore"        [:div [:p "x\nmore"]]
  "p1\n\np2\n"     [:div [:p "p1\n"] [:p "p2\n"]]
  "p1\n\n\n\np2\n" [:div [:p "p1\n"] [:p "p2\n"]])


(describe-examples vector (partial parses paragraph-parser)
  "stuff"                    ["stuff"]
  "\nmulti\nline\n"          ["\nmulti\nline\n"]
  "/italicized stuff/"       [[:em "italicized stuff"]]
  "/bang!/, he said"         [[:em "bang!"] ", he said"]
  "Awwww /shucks!/"          ["Awwww " [:em "shucks!"]]
  "text with just / one slash" ["text with just / one slash"]
  "I like *bold stuff* better" ["I like " [:strong "bold stuff"] " better"]
  "/ a * b /"                [[:em " a * b "]]
  "Visit *http://foo.com*!"  ["Visit " [:strong "http://foo.com"] "!"]
  "[[a]]"                    [[:link "a"]]
  "[[http://x.com/y]]"       [[:link "http://x.com/y"]]
  "Visit [[http://x.com/y]]" ["Visit " [:link "http://x.com/y"]]
  "[[a][b]]"                 [[:link "a" "b"]]
  "[[a][b\nc]]"              [[:link "a" "b\nc"]]
  "x [[a][b]]"               ["x " [:link "a" "b"]]
  "[[a][b]] x"               [[:link "a" "b"] " x"]
  "[[a][*b*]]"               [[:link "a" [:strong "b"]]]
  "*[[a][b]]*"               [[:strong [:link "a" "b"]]]
  "*x [[a][b]]*"             [[:strong "x " [:link "a" "b"]]]
  "[[z][link w/ *bold*]]"    [[:link "z" "link w/ " [:strong "bold"]]])


(defn- complete-parse-and-transform [txt]
  (-> txt parse-stages :with-links))


;; End-to-end parsing
(describe-examples (fn [s] [:document [:div s]]) complete-parse-and-transform
  "word"          [:p "word"]
  "line\n"        [:p "line\n"]
  "\nline"        [:p "line"]
  "\nline\n"      [:p "line\n"]
  "/italics/"     [:p [:em "italics"]]
  "/italics/\n"   [:p [:em "italics"] "\n"]
  "\n/italics/"   [:p [:em "italics"]]
  "\n/italics/\n" [:p [:em "italics"] "\n"]
  "in /italics/"  [:p "in " [:em "italics"]]
  "/em/ rocks"    [:p [:em "em"] " rocks"]
  "*strong*"      [:p [:strong "strong"]]
  "*strong*\n"    [:p [:strong "strong"] "\n"]
  "\n*strong*\n"  [:p [:strong "strong"] "\n"]
  "[[link]]"      [:p [:a {:href "link"} "link"]]
  "[[link]]\n"    [:p [:a {:href "link"} "link"] "\n"]
  "\n[[link]]"    [:p [:a {:href "link"} "link"]]
  "[[a][b]]"      [:p [:a {:href "a"} "b"]]
  "[[http://x.com][a normal link]]"
                  [:p [:a {:href "http://x.com"} "a normal link"]]
  "[[http://x.com][a link with *bold*]]"
                  [:p [:a {:href "http://x.com"}
                       "a link with " [:strong "bold"]]])
