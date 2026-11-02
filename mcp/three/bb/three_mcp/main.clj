(ns three-mcp.main
  "The three MCP server: 3D views for xi (and any MCP Apps host), drawn by
   squint under src/ on three.js. breadboard_* (three-mcp.breadboard) and
   scene_show (three-mcp.scene). A view's URI carries its bundle's hash, so
   after a source change `/mcp refresh three` hands out the new view.

   Register in ~/.config/xi/mcp.edn:
     {:three {:command \"bb\"
              :args [\"--config\" \"<xi-extensions>/mcp/three/bb.edn\" \"serve\"]
              :code-paths [\"<xi-extensions>/mcp/three/bb\" \"<xi-extensions>/mcp/three/src\"]}}"
  (:require [clojure.string :as str]
            [three-mcp.breadboard :as breadboard]
            [three-mcp.rpc :as rpc]
            [three-mcp.scene :as scene]))

(defn- tools [] (into (breadboard/tools) (scene/tools)))

(defn- resources []
  [(dissoc (breadboard/view-resource (breadboard/view-uri)) :text)
   (dissoc (scene/view-resource (scene/view-uri)) :text)])

(defn- read-resource [uri]
  (cond
    (str/starts-with? (str uri) "ui://three/breadboard/") (breadboard/view-resource uri)
    (str/starts-with? (str uri) "ui://three/scene/") (scene/view-resource uri)))

(defn -main [& _]
  (rpc/serve! {:name "three" :version "0.1.0" :tools tools :resources resources :read read-resource}))
