(ns breadboard-mcp.main
  "The breadboard MCP server: 3D views for xi (and any MCP Apps host), drawn by
   squint under src/ on three.js. breadboard_* (breadboard-mcp.breadboard) and
   scene_show (breadboard-mcp.scene). A view's URI carries its bundle's hash, so
   after a source change `/mcp refresh breadboard` hands out the new view.

   Register in ~/.config/xi/mcp.edn:
     {:breadboard {:command \"bb\"
                   :args [\"--config\" \"<xi-extensions>/mcp/breadboard/bb.edn\" \"serve\"]
                   :code-paths [\"<xi-extensions>/mcp/breadboard/bb\" \"<xi-extensions>/mcp/breadboard/src\"]}}"
  (:require [clojure.string :as str]
            [breadboard-mcp.breadboard :as breadboard]
            [breadboard-mcp.rpc :as rpc]
            [breadboard-mcp.scene :as scene]))

(defn- tools [] (into (breadboard/tools) (scene/tools)))

(defn- resources []
  [(dissoc (breadboard/view-resource (breadboard/view-uri)) :text)
   (dissoc (scene/view-resource (scene/view-uri)) :text)])

(defn- read-resource [uri]
  (cond
    (str/starts-with? (str uri) "ui://breadboard/board/") (breadboard/view-resource uri)
    (str/starts-with? (str uri) "ui://breadboard/scene/") (scene/view-resource uri)))

(defn -main [& _]
  (rpc/serve! {:name "breadboard" :version "0.1.0" :tools tools :resources resources :read read-resource}))
