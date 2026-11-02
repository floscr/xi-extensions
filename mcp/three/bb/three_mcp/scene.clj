(ns three-mcp.scene
  "scene_show: the agent describes a 3D scene as data and the chat shows it
   as an interactive view (src/scene/ui). Drag to orbit, scroll to zoom,
   click an object: the agent learns the selection through ui_state, \"Ask
   about it\" puts a question in the message box."
  (:require [clojure.string :as str]
            [three-mcp.views :as views]))

(def entry "scene.ui.main")

(defn view-uri [] (str "ui://three/scene/" (views/version entry)))

(defn view-resource [uri]
  {:uri uri :name "scene" :mimeType "text/html;profile=mcp-app"
   :_meta {:ui {:csp views/csp}}
   :text (views/view-html {:title "scene" :entry entry})})

(def ^:private shapes #{"box" "sphere" "torus" "torusKnot" "cone" "cylinder"})

(defn- object [o]
  (let [pos (:position o)
        size (:size o)]
    {:shape (if (shapes (:shape o)) (:shape o) "box")
     :position (if (and (sequential? pos) (= 3 (count pos)) (every? number? pos)) (vec pos) [0 0 0])
     :size (cond
             (number? size) size
             (and (sequential? size) (= 3 (count size)) (every? number? size)) (vec size)
             :else 1)
     :color (if (string? (:color o)) (:color o) "#8ab4f8")
     :label (if (string? (:label o)) (:label o) (str (:shape o)))}))

(defn- show [{:keys [title background objects]} _]
  (let [objs (mapv object (take 200 (if (sequential? objects) objects [])))]
    (if (empty? objs)
      {:content [{:type "text" :text "Pass at least one object in `objects`."}] :isError true}
      {:content [{:type "text"
                  :text (str "Showing " (count objs) " object" (when (not= 1 (count objs)) "s")
                             (when title (str " as \"" title "\""))
                             " in a 3D view the user can orbit and click: "
                             (str/join ", " (map :label objs))
                             ". The object they select shows up in ui_state.")}]
       :structuredContent {:scene (cond-> {:objects objs}
                                    (string? title) (assoc :title title)
                                    (string? background) (assoc :background background))}})))

(defn tools []
  [{:name "scene_show"
    :description (str "Show a 3D scene in the chat as an interactive three.js view: the user orbits, "
                      "zooms and clicks objects. Use it when a spatial arrangement explains something "
                      "better than text (layouts, structures, data in 3D). Units are arbitrary; y is up, "
                      "the floor grid is 20 x 20 around the origin.")
    :inputSchema {:type "object"
                  :properties {:title {:type "string"}
                               :background {:type "string" :description "CSS color; omit for the chat's background"}
                               :objects {:type "array"
                                         :items {:type "object"
                                                 :properties {:shape {:type "string" :enum (vec (sort shapes))}
                                                              :position {:type "array" :items {:type "number"} :description "[x, y, z]"}
                                                              :size {:description "a number, or [width, height, depth]"}
                                                              :color {:type "string"}
                                                              :label {:type "string"}}}}}
                  :required ["objects"]}
    :_meta {:ui {:resourceUri (view-uri)}}
    :handler show}])
