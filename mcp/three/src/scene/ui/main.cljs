(ns scene.ui.main
  "scene_show's view: the agent's objects as three.core hiccup on a
   three.stage over a floor grid. Clicking an object selects it and tells
   the agent (ui_state); \"Ask about it\" puts a question in the message box."
  (:require ["three" :as THREE]
            [three.core :as core]
            [three.dom :as dom]
            [three.host :as host]
            [three.stage :as stage]))

(def ^:private style "
body { font-size: 13px; }
#stage { position: absolute; inset: 0; }
canvas { display: block; width: 100%; height: 100%; cursor: grab; }
#bar { position: absolute; left: 8px; top: 8px; right: 8px; display: flex; gap: 6px; align-items: center; pointer-events: none; }
#bar > * { pointer-events: auto; }
button { font: inherit; padding: 3px 9px; border-radius: var(--border-radius-sm, 6px); cursor: pointer;
         border: 1px solid var(--color-border-primary, #8884); color: inherit;
         background: var(--color-background-secondary, #2228); }
button:disabled { opacity: .5; cursor: default; }
#info { opacity: .8; text-shadow: 0 0 4px var(--color-background-primary, #000); }
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

(defn- mount! []
  (js/document.head.append (dom/h [:style style]))
  (let [view (dom/h [:div {:id "stage"}
                     [:canvas]
                     [:div {:id "bar"}
                      [:button {:id "full"} "Full screen"]
                      [:button {:id "ask" :disabled true} "Ask about it"]
                      [:span {:id "info"} "Loading…"]]])
        _ (js/document.body.append view)
        info (fn [t] (set! (.-textContent (js/document.getElementById "info")) t))
        stg (stage/create! (.querySelector view "canvas") {:fov 50 :damping 0.08
                                                          :on-miss (fn [_] (swap! state assoc :selected nil))})
        mode (atom "inline")
        h (host/connect! {:name "scene" :height 420
                          :on-mode (fn [m] (reset! mode m))
                          :on-result (fn [sc] (swap! state assoc :scene (or (:scene sc) {}) :selected nil :fresh true))})
        label (fn [i] (:label (nth (:objects (:scene @state)) i)))
        ;; the agent's objects apart from the grid and lights, so framing fits them
        objs (THREE/Group.)]
    (core/render! (:root stg) surroundings)
    (.add (:root stg) objs)
    (.observe (js/ResizeObserver. #(stage/resize! stg (.-clientWidth view) (.-clientHeight view))) view)
    (set! (.-onclick (js/document.getElementById "full"))
          (fn [] (.then (host/display-mode! h (if (= @mode "fullscreen") "inline" "fullscreen"))
                        (fn [m] (reset! mode m)
                          (set! (.-textContent (js/document.getElementById "full"))
                                (if (= m "fullscreen") "Exit full screen" "Full screen"))))))
    (set! (.-onclick (js/document.getElementById "ask"))
          (fn [] (when-let [i (:selected @state)]
                   (host/message! h (str "Tell me about the " (label i) " in the scene.")))))
    (add-watch state :render
               (fn [_ _ before now]
                 (core/render! objs (objects now))
                 (when-let [bg (:background (:scene now))] (set! (.-background (:scene stg)) (THREE/Color. bg)))
                 (when (:fresh now)
                   (swap! state dissoc :fresh)
                   (let [box (.setFromObject (THREE/Box3.) objs)]
                     (stage/frame! stg box [1 0.8 1.2] 1.2)))
                 (let [i (:selected now)]
                   (set! (.-disabled (js/document.getElementById "ask")) (nil? i))
                   (if (some? i)
                     (do (info (str "Selected: " (label i)))
                         (when-not (= i (:selected before))
                           (host/context! h (str "In the 3D view the user selected: " (label i)))))
                     (info (str (count (:objects (:scene now))) " objects · drag to orbit, scroll to zoom, click one"))))
                 (stage/request-render! stg)))))

(mount!)
