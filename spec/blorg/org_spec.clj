(ns blorg.org-spec
  (:require [blorg.org :refer :all]
            [blorg.util :refer [vec*]]
            [speclj.core :refer :all]))


(defmacro describe-examples [right-fn left-fn & body]
  `(describe "Examples"
     ~@(for [[l r] (partition 2 body)]
         `(~'it ~l
            (~'should= (~right-fn ~r) (~left-fn ~l))))))


(describe-examples identity get-title
  "#+TITLE: a title\n"          "a title"
  "#+TITLE: a title\nx\n"       "a title"
  "yz\n\n#+TITLE: a title\nx\n" "a title")


(describe-examples identity split-headers-and-body
  "#+A\n"                       ["#+A\n" ""]
  "#+A\n#+B\n"                  ["#+A\n#+B\n" ""]
  "#+A\nStuff"                  ["#+A\n" "Stuff"]
  "\n#+A\nStuff"                ["\n#+A\n" "Stuff"]
  "#+A\n\n#+B\nStuff"           ["#+A\n\n#+B\n" "Stuff"]
  "#+A\nStuff\nMore\n"          ["#+A\n" "Stuff\nMore\n"]
  "#+A\n# comment\n#+B\nBody"   ["#+A\n# comment\n#+B\n" "Body"]
  "#+A\n#+CAPTION: jaz\n\nBody" ["#+A\n" "#+CAPTION: jaz\n\nBody"]
  "#+Z\n#+BEGIN_SRC bada\nX"    ["#+Z\n" "#+BEGIN_SRC bada\nX"]
  "#+Z\n#+ATTR_HTML bada\nX"    ["#+Z\n" "#+ATTR_HTML bada\nX"]
  "#+A\n#+HTML: jaz\n\nBody"    ["#+A\n" "#+HTML: jaz\n\nBody"]
  "#+A\n#+HTML_HEAD: jaz\n\nX"  ["#+A\n#+HTML_HEAD: jaz\n\n" "X"])


(describe-examples #(vec* :div %) convert-body-to-sections
  "* Sec1\n"                [[:h1 "Sec1"]]
  "* Sec1\n* Sec2\n"        [[:h1 "Sec1"] [:h1 "Sec2"]]
  "* Sec1\nX\n"             [[:h1 "Sec1"] "X\n"]
  "* Sec1\nX\n* Sec2\nY\n"  [[:h1 "Sec1"] "X\n" [:h1 "Sec2"] "Y\n"]
  "* Sec1\n** Sec1a\nX\n"   [[:h1 "Sec1"] [:h2 "Sec1a"] "X\n"]
  "* S1\nB1\n** S1a\nB1a\n" [[:h1 "S1"] "B1\n" [:h2 "S1a"] "B1a\n"]
  "* Sec1\nX\nY\n"          [[:h1 "Sec1"] "X\nY\n"]
  "NoSection\nL2\n"         ["NoSection\nL2\n"]
  "*bold* stuff\n* Sec1\n"  ["*bold* stuff\n" [:h1 "Sec1"]])


(describe-examples identity find-paragraphs
  "x"          [[:p "x"]]
  "x\n"        [[:p "x\n"]]
  "p1\n\np2"   [[:p "p1\n"] [:p "p2"]]
  "p1\n\np2\n" [[:p "p1\n"] [:p "p2\n"]])


(describe-examples identity linkify
  "nonlink"       ["nonlink"]
  "line1\nline2"  ["line1\nline2"]
  "[[a][b]]"      [[:a {:href "a"} "b"]]
  "zoop [[a][b]]" ["zoop " [:a {:href "a"} "b"]]
  "[[a][b]] pooz" [[:a {:href "a"} "b"] " pooz"]
  "x [[a][b]] y"  ["x " [:a {:href "a"} "b"] " y"]
  "x [[a][b]] y [[c][d]] z" ["x "
                             [:a {:href "a"} "b"]
                             " y "
                             [:a {:href "c"} "d"]
                             " z"])


(describe-examples identity boldify
  "zazza"         ["zazza"]
  "line1\nline2"  ["line1\nline2"]
  "*strong*"      [[:strong "strong"]]
  "good *stuff*"  ["good " [:strong "stuff"]]
  "*good* stuff"  [[:strong "good"] " stuff"])


(describe-examples identity emify
  "zazza"                          ["zazza"]
  "line1\nline2"                   ["line1\nline2"]
  "/em/"                           [[:em "em"]]
  "good /stuff/"                   ["good " [:em "stuff"]]
  "/good/ stuff"                   [[:em "good"] " stuff"]
  "an \"/em quote/\""              ["an \"" [:em "em quote"] "\""]
  "http://foo"                     ["http://foo"]
  "http://bit.ly/simple-made-easy" ["http://bit.ly/simple-made-easy"])


(describe-examples identity code-ify
  "aaabbb"         ["aaabbb"]
  "l1\nl2"         ["l1\nl2"]
  "=code="         [[:code "code"]]
  "a =code="       ["a " [:code "code"]]
  "=code= red"     [[:code "code"] " red"]
  "l1\nl2 =x=\n z" ["l1\nl2 " [:code "x"] "\n z"])


(describe-examples identity hr-ify
  "asdf"     ["asdf"]
  ;; "a - b"    ["a - b"]
  ;; "a -- b"   ["a &#x2013            ; b"]
  ;; "a --- b"  ["a &#x2014; b"]
  ;; "a ---- b" ["a &#x2014;- b"]
  ;; "----"     ["&#x2014;-"]
  "a\n-----\nb\n" ["a\n" [:hr] "\nb\n"])


(describe-examples identity srcify
  "asdf"                            ["asdf"]
  "#+BEGIN_SRC x\n123\n#+END_SRC\n" [[:pre {:class "lang_x"}
                                      "123\n"]])


(describe-examples identity example-ify
  "asdf"                                  ["asdf"]
  "#+BEGIN_EXAMPLE\n123\n#+END_EXAMPLE\n" [[:pre "123\n"]]
  "#+BEGIN_EXAMPLE
<hr>
<script lang=\"ada\"></script>
#+END_EXAMPLE\n"                         [[:pre (str "&lt;hr&gt;\n&lt;script "
                                                     "lang=&quot;ada&quot;&gt;"
                                                     "&lt;/script&gt;\n")]])
