(ns scene.ui.main
  "scene_show's view: the agent's objects as three.core hiccup on a
   three.stage over a floor grid. Clicking an object selects it and tells
   the agent (ui_state); \"Ask about it\" puts a question in the message box."
  (:require ["three" :as THREE]
            [three.core :as core]
            [three.dom :as dom]
            [three.host :as host]
            [three.shadow :as shadow]
            [three.stage :as stage]
            [ui.button :as button]))

(def ^:private style "
:host { position: fixed; inset: 0; background: transparent; }
.stage { position: absolute; inset: 0; }
canvas { display: block; width: 100%; height: 100%; cursor: grab; }
.bar { position: absolute; left: 8px; top: 8px; right: 8px; display: flex; gap: 6px; align-items: center; pointer-events: none; }
.bar > * { pointer-events: auto; }
.bar .btn-secondary { background: var(--bg-1); }
.info { color: var(--fg-1); padding: 2px 8px; border-radius: var(--radius-pill); background: color-mix(in oklch, var(--bg-1) 70%, transparent); }
")

(def ^:private state (atom {:scene {} :selected nil}))

(defn- shape-node [i o selected]
  (let [s (:size o)
        [w h d] (if (vector? s) s [s s s])
        tag (case (:shape o) "sphere" :sphere "torus" :torus "torusKnot" :torus-knot "cone" :cone "cylinder" :cylinder :box)
        geometry (case tag
                   :sphere {:radius (/ w 2) :segments 32}
                   :torus {:radius (/ w 2) :tube (/ w 6)}
                   :torus-knot {:radius (/ w 3) :tube (/ w 10)}
                   :cone {:radius (/ w 2) :height h :segments 32}
                   :cylinder {:radius (/ w 2) :height h :segments 32}
                   {:size [w h d]})]
    [tag (merge geometry
                {:key (str i) :position (:position o)
                 :material {:color (:color o) :roughness 0.5 :emissive (if (= i selected) "#333333" "#000000")}
                 :on-click (fn [_] (swap! state assoc :selected i))})]))

(def ^:private surroundings
  [[:hemisphere-light {:key "sky" :sky "#ffffff" :ground "#444444" :intensity 1.2}]
   [:directional-light {:key "sun" :intensity 1.5 :position [5 10 7]}]
   [:grid {:key "grid" :size 20 :divisions 20}]])

(defn- objects [{:keys [scene selected]}]
  (map-indexed (fn [i o] (shape-node i o selected)) (:objects scene)))

(defn- bar
  "Full screen, Ask about it, the info line; `mode` the display mode."
  [{:keys [scene selected]} mode on-full on-ask]
  [(button/button {:variant "secondary" :size "sm" :icon-left (if (= mode "fullscreen") "minimize" "maximize")
                   :on-click on-full}
                  (if (= mode "fullscreen") "Exit full screen" "Full screen"))
   (button/button {:variant "secondary" :size "sm" :icon-left "search" :disabled (nil? selected) :on-click on-ask}
                  "Ask about it")
   [:span {:class "info"}
    (cond (nil? (:objects scene)) "Loading…"
          (some? selected) (str "Selected: " (:label (nth (:objects scene) selected)))
          :else (str (count (:objects scene)) " objects · drag to orbit, scroll to zoom, click one"))]])

(defn- mount! []
  (let [host-el (js/document.createElement "div")
        _ (js/document.body.append host-el)
        root (shadow/attach! host-el style [:div {:class "stage"} [:canvas] [:div {:class "bar"}]])
        view (.querySelector root ".stage")
        bar-el (.querySelector root ".bar")
        stg (stage/create! (.querySelector view "canvas") {:fov 50 :damping 0.08
                                                          :on-miss (fn [_] (swap! state assoc :selected nil))})
        mode (atom "inline")
        host-ref (atom nil)
        draw-bar! (fn []
                    (dom/replace-children!
                     bar-el
                     (bar @state @mode
                          (fn [] (.then (host/display-mode! @host-ref (if (= @mode "fullscreen") "inline" "fullscreen"))
                                        (fn [m] (reset! mode m))))
                          (fn [] (when-let [i (:selected @state)]
                                   (host/message! @host-ref (str "Tell me about the " (:label (nth (:objects (:scene @state)) i)) " in the scene.")))))))
        h (host/connect! {:name "scene" :height 420
                          :on-mode (fn [m] (reset! mode m))
                          :on-result (fn [sc] (swap! state assoc :scene (or (:scene sc) {}) :selected nil :fresh true))})
        ;; the agent's objects apart from the grid and lights, so framing fits them
        objs (THREE/Group.)]
    (reset! host-ref h)
    (add-watch mode :bar (fn [_ _ _ _] (draw-bar!)))
    (draw-bar!)
    (core/render! (:root stg) surroundings)
    (.add (:root stg) objs)
    (.observe (js/ResizeObserver. #(stage/resize! stg (.-clientWidth view) (.-clientHeight view))) view)
    (add-watch state :render
               (fn [_ _ before now]
                 (core/render! objs (objects now))
                 (when-let [bg (:background (:scene now))] (set! (.-background (:scene stg)) (THREE/Color. bg)))
                 (when (:fresh now)
                   (swap! state dissoc :fresh)
                   (let [box (.setFromObject (THREE/Box3.) objs)]
                     (stage/frame! stg box [1 0.8 1.2] 1.2)))
                 (let [i (:selected now)]
                   (when (and (some? i) (not= i (:selected before)))
                     (host/context! h (str "In the 3D view the user selected: " (:label (nth (:objects (:scene now)) i))))))
                 (draw-bar!)
                 (stage/request-render! stg)))))

(mount!)
