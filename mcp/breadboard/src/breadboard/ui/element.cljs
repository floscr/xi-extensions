(ns breadboard.ui.element
  "<breadboard-view>: a layout drawn in 3D (breadboard.ui.scene) on a
   three.stage, with a bar (3D / Top / Underside / Full screen), a sidebar
   to show and hide parts and wires, an info line, and tutorial steps.

   Everything that changes what is shown goes through `command!` ({:view
   :only :show :hide :labels :highlight :focus :note :step :clear}): the
   sidebar, page links (breadboard.ui.main), the agent's commands and live
   eval (js/board in a view). The layout comes from a child <script
   type=\"application/json\">, or `setLayout`."
  (:require ["three" :as THREE]
            [breadboard.model :as model]
            [breadboard.ui.draw :as draw]
            [breadboard.ui.scene :as scene]
            [three.core :as core]
            [three.dom :as dom]
            [three.stage :as stage]
            [three.text :as text]))

(def hint "Drag to orbit, scroll to zoom. Click a hole, pin or wire to see what it connects to.")

(def style (str "
:host { display: block; position: relative; height: 440px; overflow: hidden; border-radius: 12px;
        font: 13px/1.35 " text/font "; color: var(--color-text-primary, #1d1d1f);
        background: var(--bb-background, transparent); }
.root { display: flex; height: 100%; }
.stage { position: relative; flex: 1; min-width: 0; }
canvas { display: block; width: 100%; height: 100%; cursor: grab; touch-action: none; }
.bar { position: absolute; left: 8px; top: 8px; right: 8px; display: flex; gap: 6px; align-items: center;
       pointer-events: none; }
.bar > * { pointer-events: auto; }
.title { font-weight: 650; margin-right: auto; padding: 3px 8px; border-radius: 999px; min-width: 0;
         overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
         background: var(--color-background-secondary, #ffffffd9); }
.title[hidden] + * { margin-left: auto; }
button { font: inherit; padding: 3px 10px; flex: none; border-radius: var(--border-radius-sm, 999px); cursor: pointer;
         border: 1px solid var(--color-border-primary, #0000001f); color: inherit;
         background: var(--color-background-secondary, #ffffffd9); }
button.on { background: var(--color-text-primary, #1d1d1f); color: var(--color-background-primary, #fff); }
button:disabled { opacity: .4; cursor: default; }
.info { position: absolute; left: 8px; bottom: 8px; max-width: calc(100% - 16px); padding: 4px 10px;
        border-radius: 10px; background: var(--color-background-secondary, #ffffffe6); pointer-events: none;
        white-space: pre-wrap; box-sizing: border-box; }
.info:empty { display: none; }
.problems { color: #c62828; }
.has-steps .info { bottom: 58px; }
.steps { position: absolute; left: 8px; right: 8px; bottom: 8px; display: flex; gap: 8px; align-items: center;
         padding: 6px 8px; border-radius: 12px; box-sizing: border-box;
         border: 1px solid var(--color-border-primary, #0000001f);
         background: var(--color-background-secondary, #ffffffee); }
.steps[hidden] { display: none; }
.stext { flex: 1; max-height: 3.6em; overflow: auto; }
.side { width: 230px; flex: none; overflow: auto; padding: 8px 6px 8px 8px; box-sizing: border-box; font-size: 12px;
        border-right: 1px solid var(--color-border-primary, #0000001a);
        background: var(--color-background-secondary, #f7f5ef); }
.side[hidden] { display: none; }
.side ul { list-style: none; margin: 2px 0 10px; padding: 0; }
.side li { display: flex; align-items: center; gap: 5px; padding: 1px 3px; border-radius: 6px; }
.side li.sel { background: #ffb30040; }
.side .grp { display: flex; align-items: center; gap: 6px; font-weight: 650; margin: 4px 0 2px; cursor: pointer; }
.side .sw { width: 14px; height: 7px; border-radius: 4px; flex: none; border: 1px solid #00000026; }
.side .pick { all: unset; cursor: pointer; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
              flex: 1; min-width: 0; }
.side .pick:hover b { text-decoration: underline; }
.side .sub { opacity: .65; }
.side input { margin: 0; }
"))

(defn st [el] (.-bb el))
(defn- q [el sel] (.querySelector (.-shadowRoot el) sel))
(defn- q-all [el sel] (.querySelectorAll (.-shadowRoot el) sel))

(defn say! [el text problems?]
  (let [info (q el ".info")]
    (set! (.-textContent info) (or text ""))
    (.toggle (.-classList info) "problems" (boolean problems?))))

;; ── Drawing ──

(declare on-pick!)

(defn render!
  "Re-render the scene from the element's state."
  [el]
  (let [s (st el)]
    (when (:resolved s)
      (core/render! (:pivot s)
                    [(scene/scene (:resolved s)
                                  {:hidden (:hidden s) :glow (:glow s) :rings (:rings s) :labels (:labels s)
                                   :flipped (:flipped s) :extras (:extras s)
                                   :on-pick (fn [p] (on-pick! el p))}
                                  (:textures s))])
      (stage/request-render! (:stage s)))))

(defn- content [el] (first (.-children (:pivot (st el)))))

(defn- textures [r]
  {:top (draw/board-face r false)
   :bottom (draw/board-face r true)
   :parts (into {}
                (keep (fn [p]
                        (when (seq (:pins p))
                          (case (:type p)
                            "module" [(:key p) (draw/module-top p (scene/body-box p 0.6) (or (:color (:spec p)) "#1d5fa8"))]
                            "dip" (when (some #(= 1 (:row %)) (:pins p))
                                    (let [b (scene/chip-box p)] [(:key p) (draw/chip-top p b (:dir b))]))
                            nil))))
                (:parts r))})

;; ── Framing ──

(defn frame-all!
  "Aim at everything drawn: the board, the parts and the off-board tags."
  [el]
  (let [s (st el)
        pivot (:pivot s)
        rot (.. pivot -rotation -x)]
    (set! (.. pivot -rotation -x) 0)
    (.updateMatrixWorld pivot true)
    (let [box (.setFromObject (THREE/Box3.) (content el))]
      (set! (.. pivot -rotation -x) rot)
      (.updateMatrixWorld pivot true)
      (when (:flipped s)
        (let [lo (.clone (.-min box)) hi (.clone (.-max box))]
          (.set (.-min box) (.-x lo) (- (.-y hi)) (- (.-z hi)))
          (.set (.-max box) (.-x hi) (- (.-y lo)) (- (.-z lo)))))
      (stage/frame! (:stage s) box
                    (if (contains? #{"top" "bottom"} (:view s)) [0 1 0.0001] [0 0.82 0.58])
                    1.18))))

(defn- fit! [el reset?]
  (let [s (st el) box (q el ".stage")]
    (stage/resize! (:stage s) (.-clientWidth box) (.-clientHeight box))
    (when (and reset? (:resolved s)) (frame-all! el))))

;; ── Views ──

(defn- flip! [el on instant?]
  (let [s (st el) pivot (:pivot s)
        from (.. pivot -rotation -x) to (if on js/Math.PI 0)]
    (assoc! s :flipped on)
    (render! el)
    (stage/tween! (if instant? 0 450)
                  (fn [e]
                    (set! (.. pivot -rotation -x) (+ from (* (- to from) e)))
                    (stage/request-render! (:stage s))))))

(defn set-view! [el view instant?]
  (let [s (st el)]
    (when (:resolved s)
      (let [flip? (= view "bottom")]
        (when-not (= flip? (boolean (:flipped s))) (flip! el flip? instant?))
        (assoc! s :view view)
        (doseq [b (q-all el "[data-view]")]
          (.toggle (.-classList b) "on" (= view (.. b -dataset -view))))
        (fit! el true)))))

;; ── Pointing at things ──

(defn- item [s k]
  (or (some #(when (= k (:key %)) {:kind "part" :obj %}) (:parts (:resolved s)))
      (some #(when (= k (:key %)) {:kind "wire" :obj %}) (:wires (:resolved s)))))

(defn- hole-of [s t]
  (let [r (:resolved s)]
    (or (model/hole (:board r) t) (:hole (model/pin-of (:parts r) t)))))

(defn end-name [s e]
  (if (:off e)
    (:off e)
    (let [here (for [p (:parts (:resolved s)) pin (:pins p) :when (= (:key (:hole pin)) (:key (:hole e)))]
                 (str (or (:id p) (:label p)) " " (:name pin)))]
      (str (:key (:hole e)) (when (seq here) (str " (" (.join (vec here) ", ") ")"))))))

(defn- net-holes [s net]
  (let [r (:resolved s)]
    (filterv #(= net (model/net-of r {:hole %})) (model/holes (:board r)))))

(defn- net-text
  "Who is on `net`, as the info line puts it."
  [s net]
  (let [r (:resolved s)
        members (concat (for [p (:parts r) pin (:pins p) :when (= net (model/net-of r {:hole (:hole pin)}))]
                          (str (or (:id p) (:label p)) " " (if (seq (:name pin)) (:name pin) (:key (:hole pin)))))
                        (for [w (:wires r) :when (and (:off (:to w)) (= net (model/net-of r (:from w))))]
                          (:off (:to w))))]
    (str "Connected (" (count (net-holes s net)) " holes): "
         (if (seq members) (.join (vec (distinct members)) ", ") "nothing else"))))

(defn- mark-rows! [el ids]
  (let [first-row (atom nil)]
    (doseq [li (q-all el ".side li")]
      (let [on (some #(= % (.. li -dataset -id)) ids)]
        (.toggle (.-classList li) "sel" (boolean on))
        (when (and on (nil? @first-row)) (reset! first-row li))))
    (when @first-row (.scrollIntoView @first-row {:block "nearest"}))))

(defn highlight!
  "Light up `targets` (ids, holes, part pins): wires and parts glow, wire
   ends and holes get rings. `note` replaces the info line (\"\" empties it)."
  [el targets note]
  (let [s (st el)
        lines (atom [])
        glow (atom {})
        rings (atom [])]
    (doseq [t (model/targets targets)]
      (if-let [{:keys [kind obj]} (item s t)]
        (if (= kind "wire")
          (do (swap! glow assoc t "#664400")
              (swap! rings into (keep :hole [(:from obj) (:to obj)]))
              (swap! lines conj (str "Wire " t (when (seq (:label obj)) (str " (" (:label obj) ")")) ": "
                                     (end-name s (:from obj)) " → " (end-name s (:to obj)))))
          (do (swap! glow assoc t "#3a3a3a")
              (swap! lines conj (str (or (not-empty (:label obj)) t)
                                     (when-let [o (:ohms (:spec obj))] (str " · " o " Ω"))))))
        (when-let [h (hole-of s t)]
          (let [net (model/net-of (:resolved s) {:hole h})]
            (swap! rings into (net-holes s net))
            (swap! lines conj (str t ": " (net-text s net)))))))
    (assoc! s :glow @glow :rings @rings)
    (mark-rows! el (model/targets targets))
    (let [ls @lines
          shown (if (> (count ls) 4) (conj (vec (take 3 ls)) (str "… and " (- (count ls) 3) " more")) ls)]
      (say! el (if (string? note) note (if (seq shown) (.join (vec shown) "\n") hint)) false))
    (render! el)))

(defn clear! [el]
  (assoc! (st el) :glow {} :rings [])
  (mark-rows! el [])
  (render! el))

(defn focus!
  "Fly the camera to `targets`."
  [el targets]
  (let [s (st el)
        box (THREE/Box3.)]
    (.updateMatrixWorld (:pivot s) true)
    (doseq [t (model/targets targets)]
      (if-let [obj (when-let [it (item s t)]
                     (some (fn [o] (when (= (.. o -userData -props -key) (str (:kind it) ":" t)) o))
                           (.-children (content el))))]
        (.traverse obj (fn [o] (when (and (.-geometry o) (not (.-isSprite o))) (.expandByObject box o))))
        (when-let [h (hole-of s t)]
          (.expandByPoint box (.localToWorld (content el) (THREE/Vector3. (:x h) 0 (:z h)))))))
    (stage/focus! (:stage s) box 16)))

(defn- on-pick! [el {:keys [kind key hit]}]
  (let [s (st el)
        r (:resolved s)
        nearest (fn []
                  (let [p (.worldToLocal (content el) (.clone (:point hit)))]
                    (first (sort-by #(js/Math.hypot (- (:x %) (.-x p)) (- (:z %) (.-z p)))
                                    (filter #(< (js/Math.hypot (- (:x %) (.-x p)) (- (:z %) (.-z p))) 0.8)
                                            (model/holes (:board r)))))))
        [text net glow marked]
        (case kind
          "wire" (let [w (:obj (item s key))]
                   [(str "Wire " key (when (seq (:label w)) (str " (" (:label w) ")")) ": "
                         (end-name s (:from w)) " → " (end-name s (:to w)))
                    (model/net-of r (:from w)) {} [key]])
          "part" (let [p (:obj (item s key))
                       h (nearest)
                       pin (when h (some #(when (= (:key h) (:key (:hole %))) %) (:pins p)))]
                   (if pin
                     [(str (or (not-empty (:label p)) key) " " (:name pin) " at " (:key h))
                      (model/net-of r {:hole (:hole pin)}) {} [key]]
                     [(str (or (not-empty (:label p)) key)
                           (when-let [o (:ohms (:spec p))] (str " · " o " Ω"))
                           " · pins " (.join (vec (map #(str (when (seq (:name %)) (str (:name %) " ")) (:key (:hole %))) (:pins p))) ", "))
                      nil {key "#2a2a2a"} [key]]))
          "board" (if-let [h (nearest)]
                    (let [here (for [p (:parts r) pin (:pins p) :when (= (:key h) (:key (:hole pin)))]
                                 (str (or (not-empty (:label p)) (:key p)) " " (:name pin)))]
                      [(str "Hole " (:key h) (when (seq here) (str " · " (.join (vec here) ", "))))
                       (model/net-of r {:hole h}) {} []])
                    ["" nil {} []]))
        net-glow (when net
                   (into {} (for [w (:wires r) :when (= net (model/net-of r (:from w)))] [(:key w) "#664400"])))]
    (assoc! s :sticky [] :glow (merge glow net-glow) :rings (if net (net-holes s net) []))
    (mark-rows! el marked)
    (let [full (if net (str text "\n" (net-text s net)) text)]
      (say! el (if (seq full) full hint) false)
      (render! el)
      (.dispatchEvent el (js/CustomEvent. "bb-select" {:detail {:text full} :bubbles true :composed true})))))

;; ── Sidebar ──

(defn- sync-sidebar! [el]
  (let [s (st el)]
    (doseq [cb (q-all el "input[data-id]")]
      (set! (.-checked cb) (not (contains? (:hidden s) (.. cb -dataset -id)))))
    (doseq [all (q-all el "input[data-all]")]
      (let [boxes (vec (q-all el (str "ul[data-kind=" (.. all -dataset -all) "] input")))
            on (count (filter #(.-checked %) boxes))]
        (set! (.-checked all) (= on (count boxes)))
        (set! (.-indeterminate all) (and (pos? on) (< on (count boxes))))))
    (when-let [lab (q el "input[data-labels]")]
      (set! (.-checked lab) (boolean (:labels s))))))

(declare command!)

(defn- set-shown! [el ids on]
  (let [s (st el)]
    (assoc! s :hidden (reduce (fn [h id] (if on (disj h id) (conj h id))) (:hidden s) ids))
    (sync-sidebar! el)
    (render! el)))

(defn- sidebar! [el]
  (let [s (st el)
        r (:resolved s)
        group (fn [title kind items]
                (when (seq items)
                  (let [ids (mapv :key items)]
                    [[:label {:class "grp"}
                      [:input {:type "checkbox" :data-all kind
                               :on-change (fn [e] (set-shown! el ids (.. e -target -checked)))}]
                      [:span (str title " (" (count ids) ")")]]
                     [:ul {:data-kind kind}
                      (for [it items]
                        (let [k (:key it)
                              nm (if (= kind "wire") k (or (not-empty (:label it)) k))
                              sub (if (= kind "wire")
                                    (str " " (if (seq (:label it)) (:label it) (str (end-name s (:from it)) " → " (end-name s (:to it)))))
                                    (if (and (:id it) (seq (:label it)) (not= (:label it) (:id it))) (str " " (:id it)) ""))]
                          [:li {:data-id k}
                           [:input {:type "checkbox" :data-id k
                                    :on-change (fn [e] (set-shown! el [k] (.. e -target -checked)))}]
                           [:span {:class "sw" :style {"background" (or (:color (:spec it)) (if (= kind "wire") "#2e7d32" "#9e9e9e"))}}]
                           [:button {:class "pick" :title (str nm sub)
                                     :on-click (fn [_] (command! el {:highlight [k]}))}
                            [:b nm] [:span {:class "sub"} sub]]]))]])))]
    (dom/replace-children!
     (q el ".side")
     (concat (group "Parts" "part" (:parts r))
             (group "Wires" "wire" (:wires r))
             [[:label {:class "grp"}
               [:input {:type "checkbox" :data-labels "1"
                        :on-change (fn [e] (assoc! s :labels (.. e -target -checked)) (render! el))}]
               [:span "Labels"]]]))
    (sync-sidebar! el)))

(defn toggle-sidebar! [el open]
  (let [s (st el)
        open (if (nil? open) (not (:side-open s)) (boolean open))]
    (assoc! s :side-open open)
    (set! (.-hidden (q el ".side")) (not open))
    (.toggle (.-classList (q el "[data-act=side]")) "on" open)
    (fit! el false)))

;; ── Steps ──

(defn- step-ui! [el]
  (let [s (st el)
        steps (:steps s)
        box (q el ".steps")]
    (set! (.-hidden box) (empty? steps))
    (.toggle (.-classList (q el ".stage")) "has-steps" (boolean (seq steps)))
    (when (seq steps)
      (let [k (:step s)
            step (when (>= k 0) (nth steps k))]
        (dom/replace-children!
         (q el ".stext")
         [[:b (if step (str (inc k) "/" (count steps) " · " (or (:title step) "")) (str (count steps) " steps"))]
          [:span (if step (if (:text step) (str " · " (:text step)) "") " · › builds it up step by step")]])
        (set! (.-disabled (q el "[data-step=prev]")) (< k 0))
        (set! (.-disabled (q el "[data-step=next]")) (>= k (dec (count steps))))))))

(defn go-step!
  "Step `k` (0-based; -1 is the whole board). A step's :add puts parts and
   wires on the board (hidden in the steps before) and lights them up."
  [el k quiet?]
  (let [s (st el)
        steps (:steps s)]
    (when (seq steps)
      (let [k (max -1 (min (dec (count steps)) k))
            added (set (mapcat #(model/targets (:add %)) steps))
            shown (set (mapcat #(model/targets (:add %)) (take (inc k) steps)))
            step (when (>= k 0) (nth steps k))]
        (assoc! s :step k
                :hidden (if (neg? k) #{} (set (remove #(contains? shown %) added)))
                :sticky [])
        (if step
          (command! el (cond-> (dissoc step :title :text :add :step)
                         (and (nil? (:highlight step)) (:add step)) (assoc :highlight (:add step))
                         (nil? (:note step)) (assoc :note "")))
          (do (clear! el) (sync-sidebar! el) (say! el hint false)))
        (step-ui! el)
        (when-not quiet?
          (.dispatchEvent el (js/CustomEvent. "bb-step"
                                              {:detail {:step (inc k) :of (count steps)
                                                        :title (if step (or (:title step) "") "the whole board")
                                                        :text (if step (or (:text step) "") "")}
                                               :bubbles true :composed true})))))))

;; ── Commands ──

(defn command!
  "Change what is shown: {:view :only :show :hide :labels :highlight :focus
   :note :step :clear}; lists are vectors or ids separated by spaces. `only`
   keeps just the listed parts and wires (parts stay unless parts are listed)."
  [el cmd]
  (let [s (st el)]
    (if-not (:resolved s)
      (assoc! s :pending (merge (:pending s) cmd))
      (do
        (when (:clear cmd)
          (assoc! s :hidden #{} :sticky [])
          (clear! el)
          (say! el hint false))
        (when (and (some? (:step cmd)) (seq (:steps s)))
          (go-step! el (dec (:step cmd)) true))
        (when (and (:view cmd) (not= (:view cmd) (:view s)))
          (set-view! el (:view cmd) false))
        (let [ids (fn [v] (filterv #(item s %) (model/targets v)))]
          (when (:only cmd)
            (let [keep (ids (:only cmd))
                  kinds (set (map #(:kind (item s %)) keep))
                  all (concat (map (fn [p] [(:key p) "part"]) (:parts (:resolved s)))
                              (map (fn [w] [(:key w) "wire"]) (:wires (:resolved s))))]
              (assoc! s :hidden (set (for [[k kind] all
                                           :when (and (contains? kinds kind) (not (some #(= k %) keep)))]
                                       k)))))
          (assoc! s :hidden (reduce disj (:hidden s) (ids (:show cmd))))
          (assoc! s :hidden (reduce conj (:hidden s) (ids (:hide cmd)))))
        (when (some? (:labels cmd)) (assoc! s :labels (boolean (:labels cmd))))
        (sync-sidebar! el)
        (cond
          (:highlight cmd) (do (assoc! s :sticky (model/targets (:highlight cmd)) :sticky-note (:note cmd))
                               (highlight! el (:highlight cmd) (:note cmd)))
          (some? (:note cmd)) (do (say! el (:note cmd) false) (render! el))
          :else (render! el))
        (when (:focus cmd) (focus! el (:focus cmd)))))))

(defn preview!
  "A passing highlight (hovering a link); nil puts the lasting one back."
  [el targets]
  (let [s (st el)]
    (when (:resolved s)
      (cond
        targets (highlight! el targets nil)
        (seq (:sticky s)) (highlight! el (:sticky s) (:sticky-note s))
        :else (do (clear! el) (say! el hint false))))))

;; ── Building ──

(defn build! [el]
  (let [s (st el)
        layout (or (:layout s) {})
        r (model/resolve-layout layout)]
    (assoc! s :resolved r
            :textures (assoc (textures r) :bands-of
                             (into {} (for [p (:parts r) :when (= "resistor" (:type p))] [(:key p) (draw/resistor-bands (:spec p))])))
            :hidden #{} :glow {} :rings [] :sticky [] :step -1
            :steps (if (vector? (:steps layout)) (:steps layout) [])
            :extras (or (:extras s) []))
    (set! (.-resolved el) r)
    (let [t (q el ".title")]
      (set! (.-textContent t) (or (:title layout) ""))
      (set! (.-hidden t) (not (:title layout))))
    (sidebar! el)
    (when (nil? (:side-open s))
      (toggle-sidebar! el (if (.hasAttribute el "sidebar")
                            (not= "closed" (.getAttribute el "sidebar"))
                            (>= (.-clientWidth el) 720))))
    (step-ui! el)
    (say! el (if (seq (:problems r)) (.join (vec (:problems r)) "\n") hint) (seq (:problems r)))
    (render! el)
    (set-view! el (or (:view layout) (.getAttribute el "view") "3d") true)
    (when-let [p (:pending s)]
      (assoc! s :pending nil)
      (command! el p))))

(def ^:private template
  [:div {:class "root"}
   [:aside {:class "side" :hidden true}]
   [:div {:class "stage"}
    [:canvas]
    [:div {:class "bar"}
     [:button {:data-act "side"} "Layers"]
     [:span {:class "title" :hidden true}]
     [:button {:data-view "3d"} "3D"]
     [:button {:data-view "top"} "Top"]
     [:button {:data-view "bottom"} "Underside"]
     [:button {:data-act "full"} "Full screen"]]
    [:div {:class "info"}]
    [:div {:class "steps" :hidden true}
     [:button {:data-step "prev"} "‹"]
     [:div {:class "stext"}]
     [:button {:data-step "next"} "›"]]]])

(defn connect! [el]
  (when-not (st el)
    (let [root (.attachShadow el {:mode "open"})
          css (js/document.createElement "style")]
      (set! (.-textContent css) style)
      (.append root css (dom/h template))
      (let [canvas (.querySelector root "canvas")
            stg (stage/create! canvas {:damping 0.3})
            pivot (THREE/Group.)
            s {:stage stg :pivot pivot :labels true :hidden #{} :glow {} :rings [] :sticky [] :step -1 :steps []}]
        (set! (.-bb el) s)
        (set! (.-stage el) stg)
        (.add (:root stg) pivot)
        (.add (:scene stg) (core/render! (THREE/Group.) scene/lights))
        (.observe (js/ResizeObserver. #(fit! el false)) (.querySelector root ".stage"))
        (doseq [b (.querySelectorAll root "[data-view]")]
          (.addEventListener b "click" #(set-view! el (.. b -dataset -view) false)))
        (.addEventListener (.querySelector root "[data-act=side]") "click" #(toggle-sidebar! el nil))
        (.addEventListener (.querySelector root "[data-act=full]") "click"
                           #(if-let [f (.-onFullscreen el)]
                              (f)
                              (if js/document.fullscreenElement (js/document.exitFullscreen) (.requestFullscreen el))))
        (.addEventListener (.querySelector root "[data-step=prev]") "click" #(go-step! el (dec (:step (st el))) false))
        (.addEventListener (.querySelector root "[data-step=next]") "click" #(go-step! el (inc (:step (st el))) false))
        (when-let [h (.getAttribute el "height")]
          (set! (.. el -style -height) (if (re-matches #"[0-9]+" h) (str h "px") h)))
        (when-not (:layout s)
          (when-let [json (some #(when (and (= "SCRIPT" (.-tagName %)) (= "application/json" (.-type %))) %)
                                (vec (.-children el)))]
            (try (assoc! s :layout (js/JSON.parse (.-textContent json)))
                 (catch :default e (say! el (str "Layout is not valid JSON: " (.-message e)) true)))))
        (when-let [l (.-pendingLayout el)] (assoc! s :layout l))
        (when (:layout s) (build! el))))))
