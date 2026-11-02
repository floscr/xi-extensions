(ns three-mcp.breadboard
  "The breadboard tools: a layout (board, parts, wires, steps) shown as a 3D
   view in the chat or put into an HTML document, the agent pointing at
   things in open views (breadboard_target) or running squint in them
   (breadboard_eval). Layouts are checked with breadboard.model, the same
   code the view draws with (src/breadboard)."
  (:require [babashka.fs :as fs]
            [breadboard.model :as model]
            [cheshire.core :as json]
            [clojure.string :as str]
            [three-mcp.channels :as ch]
            [three-mcp.views :as views]))

(def entry "breadboard.ui.main")

(defn view-uri [] (str "ui://three/breadboard/" (views/version entry)))

(defn view-resource [uri]
  {:uri uri :name "breadboard" :mimeType "text/html;profile=mcp-app"
   :_meta {:ui {:csp views/csp :prefersBorder true}}
   :text (views/view-html {:title "breadboard" :entry entry
                           :style "breadboard-view { height: 100%; border-radius: 0; }"
                           :body "<breadboard-view></breadboard-view>"
                           :boot "globalThis.BREADBOARD_VIEW = true;"})})

;; ── HTML documents ──
;; A board sits between `<!-- breadboard:NAME -->` and `<!-- /breadboard:NAME -->`;
;; the import map and the renderer once per page between the `script` markers.

(defn- open-marker [name] (str "<!-- breadboard:" name " -->"))
(defn- close-marker [name] (str "<!-- /breadboard:" name " -->"))

(defn board-block [name layout height]
  (str (open-marker name) "\n"
       "<breadboard-view name=\"" name "\" height=\"" height "\"><script type=\"application/json\">"
       (str/replace (json/generate-string layout) "<" "\\u003c")
       "</script></breadboard-view>\n"
       (close-marker name)))

(defn script-block []
  (str (open-marker "script") "\n" views/importmap "\n" (views/module-script (views/bundle entry)) "\n"
       (close-marker "script")))

(defn- splice
  "Replace the NAME block in html, or a bare NAME marker; nil when neither."
  [html name block]
  (let [open (open-marker name)
        i (str/index-of html open)]
    (when i
      (let [close (close-marker name)
            j (str/index-of html close i)]
        (if j
          (str (subs html 0 i) block (subs html (+ j (count close))))
          (str (subs html 0 i) block (subs html (+ i (count open)))))))))

(defn with-script
  "html with the current renderer block, replacing an older one."
  [html]
  (let [block (script-block)]
    (or (splice html "script" block)
        (if-let [i (str/last-index-of html "</body>")]
          (str (subs html 0 i) block "\n" (subs html i))
          (str html "\n" block "\n")))))

(defn put-board
  "{:html …} with the board block in place and the current renderer;
   {:error …} when html has no place for it."
  [html name block]
  (if-let [placed (splice html name block)]
    {:html (with-script placed)}
    {:error (str "No place for the board: put " (open-marker name)
                 " where it should go (or use another name), then call again.")}))

(defn- escape [s]
  (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")))

(defn page [title block]
  (str "<!doctype html>\n<html lang=\"en\"><head><meta charset=\"utf-8\">"
       "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">\n"
       "<title>" (escape title) "</title>\n"
       "<style>body{margin:0;font:16px/1.5 system-ui,sans-serif;color:#1d1d1f;background:#fafaf7}"
       "main{max-width:1000px;margin:0 auto;padding:16px}"
       "breadboard-view{border:1px solid #e5e2d8;background:#fff}</style>\n"
       "</head><body><main>\n<h1>" (escape title) "</h1>\n" block "\n</main>\n"
       (script-block) "\n</body></html>\n"))

;; ── Tools ──

(defn- error [text] {:content [{:type "text" :text text}] :isError true})

(defn- invalid [problems]
  (error (str "The layout doesn't draw:\n- " (str/join "\n- " problems))))

(defn- summary [layout]
  (let [b (:board layout)]
    (str (if (map? b) (or (:type b) "half") (or b "half")) " board, "
         (count (:parts layout)) " parts, " (count (:wires layout)) " wires")))

(def ^:private view-text
  (str "The user can orbit it, look from the top or the underside, show and hide parts and wires in its "
       "sidebar, and click a hole, pin or wire to light up what it connects to; clicks and tutorial steps "
       "show up in ui_state. Point at things in it with breadboard_target, run squint in it with "
       "breadboard_eval."))

(defn- show [{:keys [layout]} _]
  (let [problems (model/problems layout)]
    (if (seq problems)
      (invalid problems)
      {:content [{:type "text"
                  :text (str "Showing the " (summary layout)
                             (when (:id layout) (str " (board \"" (:id layout) "\")"))
                             " in a 3D view. " view-text)}]
       :structuredContent {:layout layout}})))

(defn- write-html [{:keys [layout file name height title]} meta]
  (let [problems (model/problems layout)
        name (or name "main")
        height (or height 480)
        cwd (get meta "xi/cwd")
        path (when file (if (fs/absolute? file) (fs/path file) (fs/path (or cwd ".") file)))]
    (cond
      (seq problems) (invalid problems)
      (not (re-matches #"[a-z0-9_-]+" name)) (error "name: lowercase letters, digits, - and _ only")
      (= name "script") (error "name: \"script\" is taken by the renderer's block")
      (not (str/ends-with? (str file) ".html")) (error "file: an .html path")
      :else
      (let [block (board-block name layout height)
            done (fn [text] {:content [{:type "text" :text (str text " The chat shows a preview. " view-text)}]
                             :structuredContent {:layout (update layout :id #(or % name))}})]
        (if-not (fs/exists? path)
          (do (spit (fs/file path) (page (or title (:title layout) "Breadboard") block))
              (done (str "Created " path " with the board \"" name "\" (" (summary layout) "). Write the rest "
                         "of the page around its <!-- breadboard:" name " --> block.")))
          (let [{:keys [html error]} (put-board (slurp (fs/file path)) name block)]
            (if error
              {:content [{:type "text" :text error}] :isError true}
              (do (spit (fs/file path) html)
                  (done (str "Put the board \"" name "\" (" (summary layout) ") into " path
                             ", with the current renderer."))))))))))

(def ^:private command-keys [:view :only :show :hide :labels :highlight :focus :note :step :clear])

(defn- target [{:keys [board] :as args} meta]
  (let [cmd (select-keys args command-keys)]
    (if (empty? cmd)
      (error (str "Say what to do: " (str/join ", " (map name command-keys))))
      (if (ch/send! "breadboard" (get meta "xi/roomId") board cmd)
        (str "Sent to the breadboard view" (when board (str " \"" board "\"")) " in this chat.")
        "No breadboard view in this chat is listening right now; the next one that opens starts from it."))))

(defn- run-eval [{:keys [board code]} meta]
  (if (str/blank? code)
    (error "code: the squint to run")
    (let [r (ch/eval! "breadboard" (get meta "xi/roomId") board code 30000)]
      (cond
        (nil? r) (error "No breadboard view in this chat answered within 30 s (none open, or the code hangs).")
        (:error r) (error (str "Error: " (:error r)))
        :else (str (:value r))))))

(def layout-doc
  (str "A breadboard layout, hole by hole. Holes are named row + column. Breadboards (board \"full\" 63, "
       "\"half\" 30, \"mini\" 17 columns, or {type, cols, split_rails}) have rows a-e on the near half and "
       "f-j on the far half, the trench between e and f, and the rails T+ T- (far edge) and B+ B- (near "
       "edge): \"e12\", \"T+5\". In each column a-e are connected, so are f-j; each rail is one strip. "
       "Perfboards and stripboards ({type, cols, rows}) name rows a, b, c… from the near edge; a "
       "stripboard's rows are strips.\n"
       "Parts (`type`; `id` to refer to it and its pins, `label`, `color`):\n"
       "- module: a board on pin headers (Arduino, MP3 player, sensor). pins: rows of {from, to, names}, "
       "names in order from→to. marks: [{side: left|right|top|bottom, text}] draws a connector (USB, SD) "
       "at that edge; extend: {left, right, top, bottom} grows the body past its pins, in pitches.\n"
       "- dip: a chip, pin 1 at `at` (notch on its left when its row is the near one), pins (8), "
       "names, across (row of the other side; default across the trench).\n"
       "- resistor {from, to, ohms → colour bands, or bands}, led {anode, cathode, color, lit}, "
       "diode {anode, cathode}, capacitor {from (+), to, kind electrolytic|ceramic}, button {from, to}, "
       "header {from, to, names}.\n"
       "Wires: {id, from, to, color, label, side: top|bottom}. An end is a hole, a part pin \"ID.PIN\" "
       "(\"NANO.D10\", \"U1.3\"), or {label, side} for a lead off the board (wires to the same label "
       "share one tag). Give wires ids (\"D3\") to point at them; without one a wire is wire-N, a part "
       "part-N.\n"
       "view: \"3d\" (default), \"top\", or \"bottom\" (the solder side, rows mirrored, left and right kept).\n"
       "steps (a build tutorial, Prev/Next in the view): [{title, text, add, highlight, focus, view, only, "
       "show, hide}]. A step's `add` (ids) puts parts and wires on the board, hidden in the steps before, "
       "and lights them up; the first screen shows the whole board."))

(def ^:private layout-schema
  {:type "object"
   :description layout-doc
   :properties {:id {:type "string" :description "Names the board for breadboard_target / breadboard_eval"}
                :title {:type "string"}
                :board {:description "\"full\" | \"half\" | \"mini\" | {type: breadboard size | perfboard | stripboard, cols, rows, color, split_rails}"}
                :view {:type "string" :enum ["3d" "top" "bottom"]}
                :parts {:type "array" :items {:type "object"}}
                :wires {:type "array" :items {:type "object"}}
                :steps {:type "array" :items {:type "object"}}}
   :required ["board"]})

(def ^:private ids-doc "ids of parts or wires, holes (\"j3\") or part pins (\"NANO.D10\")")

(defn tools []
  (let [ui {:ui {:resourceUri (view-uri)}}]
    [{:name "breadboard_show"
      :description (str "Show a breadboard, perfboard or stripboard with its parts and wires in the chat, "
                         "as an interactive 3D view: the user orbits it, switches to the top view or the "
                         "underside (solder side), shows and hides parts and wires, and clicks a hole, pin or "
                         "wire to see what it connects to. Use it for wiring a circuit hole by hole; give it "
                         "steps for a build tutorial. For a document, use breadboard_html.")
      :inputSchema {:type "object" :properties {:layout layout-schema} :required ["layout"]}
      :_meta ui
      :handler show}
     {:name "breadboard_html"
      :description (str "Put an interactive 3D breadboard view (the one breadboard_show draws) into an HTML "
                        "document, and preview it in the chat. A new file becomes a page with the board; in an "
                        "existing file the board replaces the block `<!-- breadboard:NAME -->…<!-- "
                        "/breadboard:NAME -->`, or a bare `<!-- breadboard:NAME -->` placeholder you put where "
                        "it should go. The renderer is inlined once per page (three.js and squint from "
                        "jsDelivr, so the page needs the internet). Call again with the same name to update.\n"
                        "Anything in the page can drive a board on click: attributes data-bb-highlight, "
                        "data-bb-focus, data-bb-only, data-bb-show, data-bb-hide (ids separated by spaces), "
                        "data-bb-view, data-bb-step, data-bb-note; data-breadboard=\"NAME\" on it or an "
                        "ancestor picks the board (default the first). Hovering a data-bb-highlight element "
                        "lights its targets up, e.g. <tr data-breadboard=\"main\" data-bb-highlight=\"D3\"> in a "
                        "wire table.")
      :inputSchema {:type "object"
                    :properties {:layout layout-schema
                                 :file {:type "string" :description "The .html file, relative to the chat's directory"}
                                 :name {:type "string" :description "Which board in the file; default \"main\""}
                                 :height {:type "number" :description "Height in px; default 480"}
                                 :title {:type "string" :description "Page title for a new file; default the layout's"}}
                    :required ["layout" "file"]}
      :_meta ui
      :handler write-html}
     {:name "breadboard_target"
      :description (str "Point at things in the breadboard views open in this chat (breadboard_show, "
                        "breadboard_html previews), for explaining or an interactive tutorial: light up parts, "
                        "wires and what they connect to, fly the camera to them, show only some, switch the "
                        "view, go to a tutorial step. Applies at once; a view opened later starts from the "
                        "last state. For HTML documents use the data-bb-* attributes instead.")
      :inputSchema {:type "object"
                    :properties {:board {:type "string" :description "The layout's id; omit for every view in the chat"}
                                 :highlight {:type "array" :items {:type "string"} :description (str "Light up these: " ids-doc)}
                                 :focus {:type "array" :items {:type "string"} :description (str "Fly the camera to these: " ids-doc)}
                                 :only {:type "array" :items {:type "string"} :description "Show only these parts/wires (parts stay unless parts are listed)"}
                                 :show {:type "array" :items {:type "string"}}
                                 :hide {:type "array" :items {:type "string"}}
                                 :view {:type "string" :enum ["3d" "top" "bottom"]}
                                 :step {:type "number" :description "Go to this tutorial step (1-based; 0 is the whole board)"}
                                 :labels {:type "boolean"}
                                 :note {:type "string" :description "Text in the view's info line"}
                                 :clear {:type "boolean" :description "Everything shown, nothing lit, before the rest"}}}
      :handler target}
     {:name "breadboard_eval"
      :description (str "Run squint (ClojureScript compiled in the page) in the breadboard view open in this chat "
                        "and get its value back: a live REPL into the view, for what breadboard_target can't do. "
                        "js/board is the <breadboard-view> element: (.command js/board {:highlight [\"D3\"]}), "
                        "(.add js/board [:sphere {:radius 1 :position [0 4 0] :material {:color \"red\"}}]) draws "
                        "three.core hiccup over the board (.clearExtras to remove), (.-stage js/board) has "
                        ":scene :camera :controls, (.-resolved js/board) the layout as the view sees it. "
                        "(require '[\"three\" :as THREE]) works. defs persist between calls.")
      :inputSchema {:type "object"
                    :properties {:code {:type "string" :description "squint forms"}
                                 :board {:type "string" :description "The layout's id; omit for any view in the chat"}}
                    :required ["code"]}
      :handler run-eval}
     {:name "breadboard_wait"
      :description "The views' long poll for breadboard_target's commands."
      :inputSchema {:type "object" :properties {:board {:type "string"} :after {:type "number"}}}
      :_meta {:ui {:visibility ["app"]}}
      :handler (fn [{:keys [board after]} meta]
                 (ch/wait "breadboard" (get meta "xi/roomId") (not-empty board) after 100000))}
     {:name "breadboard_eval_result"
      :description "A view's answer to breadboard_eval."
      :inputSchema {:type "object" :properties {:id {:type "string"} :value {:type "string"} :error {:type "string"}}}
      :_meta {:ui {:visibility ["app"]}}
      :handler (fn [{:keys [id value error]} _]
                 (ch/eval-result! id (if error {:error error} {:value value}))
                 "ok")}]))
