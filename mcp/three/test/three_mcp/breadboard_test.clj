(ns three-mcp.breadboard-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [three-mcp.breadboard :as bb]))

(def html "<html><body><h1>x</h1>\n<!-- breadboard:a -->\n<p>end</p></body></html>")

(deftest documents
  (with-redefs [bb/script-block (constantly "<!-- breadboard:script -->JS<!-- /breadboard:script -->")]
    (testing "a placeholder becomes the board, the renderer goes before </body>"
      (let [{:keys [html]} (bb/put-board html "a" (bb/board-block "a" {:board "half" :title "<x>"} 400))]
        (is (str/includes? html "<breadboard-view name=\"a\" height=\"400\">"))
        (is (str/includes? html "\\u003cx>") "no < inside the JSON")
        (is (str/includes? html "JS<!-- /breadboard:script -->\n</body>"))
        (testing "and the next call replaces the block, keeping one renderer"
          (let [again (:html (bb/put-board html "a" (bb/board-block "a" {:board "full"} 300)))]
            (is (= 1 (count (re-seq #"<breadboard-view" again))))
            (is (str/includes? again "height=\"300\""))
            (is (= 1 (count (re-seq #"breadboard:script -->JS" again))))))))
    (testing "no marker, no board"
      (is (:error (bb/put-board html "b" "B"))))))
