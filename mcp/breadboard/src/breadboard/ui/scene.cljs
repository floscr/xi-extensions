(ns breadboard.ui.scene
  "A resolved layout (breadboard.model) and the view's state as three.core
   hiccup. What the user toggles or the agent points at (hidden ids, glow,
   rings, labels, the flipped board) is data here; the element re-renders.

   state: {:hidden #{key} :glow {key colour} :rings [hole] :labels bool
           :flipped bool :extras hiccup :on-pick (fn [pick])}
   textures: {:top :bottom :parts {key texture} :bands-of {key [colour]}}
   (breadboard.ui.draw)."
  (:require [three.core :as core]))

(def ^:private PI js/Math.PI)
(def ^:private metal {:color "#b9bdc3" :metalness 0.8 :roughness 0.3})
(def ^:private gold {:color "#d4af37" :metalness 0.8 :roughness 0.3})

(defn- at [h y] [(:x h) y (:z h)])

(defn- lead [a b radius material]
  (let [{:keys [position quaternion length]} (core/between a b)]
    [:cylinder {:position position :quaternion quaternion :radius radius :height length :segments 12
                :material material}]))

(defn- label-shown? [state side]
  (and (:labels state)
       (or (= side "off") (= (= side "bottom") (boolean (:flipped state))))))

(defn- label [state side text size position opts]
  [:sprite (merge {:text text :size size :position position :render-order 2
                   :visible (label-shown? state side)}
                  opts)])

(defn body-box
  "The outline of a part around its pins, `pad` pitches out, grown by its
   :extend {:left :right :top :bottom}."
  [part pad]
  (let [xs (map #(:x (:hole %)) (:pins part))
        zs (map #(:z (:hole %)) (:pins part))
        e (or (:extend (:spec part)) {})]
    {:x0 (- (apply min xs) pad (or (:left e) 0)) :x1 (+ (apply max xs) pad (or (:right e) 0))
     :z0 (- (apply min zs) pad (or (:top e) 0)) :z1 (+ (apply max zs) pad (or (:bottom e) 0))}))

(defn chip-box
  "A chip's body: over its near row's span, between its two rows; and which
   way pin 1 counts (1 rightwards, -1 leftwards)."
  [part]
  (let [near (filter #(zero? (:row %)) (:pins part))
        far (filter #(= 1 (:row %)) (:pins part))
        xs (map #(:x (:hole %)) near)
        zn (:z (:hole (first near))) zf (:z (:hole (first far)))]
    {:x0 (- (apply min xs) 0.6) :x1 (+ (apply max xs) 0.6)
     :z0 (+ (min zn zf) 0.4) :z1 (- (max zn zf) 0.4)
     :dir (if (and (> (count near) 1) (< (:x (:hole (second near))) (:x (:hole (first near))))) -1 1)
     :turned (< zn zf)}))

;; ── Parts ──

(defn- module-node [part glow texture]
  (let [s (:spec part)
        lift (or (:lift s) 1.0) th 0.22
        color (or (:color s) "#1d5fa8")
        {:keys [x0 x1 z0 z1] :as box} (body-box part 0.6)
        w (- x1 x0) d (- z1 z0) cx (/ (+ x0 x1) 2) cz (/ (+ z0 z1) 2)
        rows (vals (group-by :row (:pins part)))]
    [:group {}
     [:box {:size [w th d] :position [cx (+ lift (/ th 2)) cz] :material {:color color :emissive glow}}]
     [:plane {:size [w d] :rotation [(/ PI -2) 0 0] :position [cx (+ lift th 0.004) cz]
              :material {:map texture :roughness 0.85 :emissive glow}}]
     ;; the headers it stands on, one black strip per pin row
     (for [pins rows]
       (let [xs (map #(:x (:hole %)) pins) zs (map #(:z (:hole %)) pins)
             lo-x (apply min xs) hi-x (apply max xs) lo-z (apply min zs) hi-z (apply max zs)]
         [:box {:size [(* 0.98 (+ 1 (- hi-x lo-x))) lift (* 0.98 (+ 1 (- hi-z lo-z)))]
                :position [(/ (+ lo-x hi-x) 2) (/ lift 2) (/ (+ lo-z hi-z) 2)]
                :material {:color "#1a1a1a" :emissive glow}}]))
     (for [m (or (:marks s) [])]
       (let [side (or (:side m) "left") horiz (or (= side "left") (= side "right"))]
         [:box {:size (if horiz [1.8 0.8 2.4] [2.4 0.8 1.8])
                :position [(case side "left" (+ x0 0.5) "right" (- x1 0.5) cx)
                           (+ lift th 0.4)
                           (case side "top" (+ z0 0.5) "bottom" (- z1 0.5) cz)]
                :material (assoc metal :color "#c3c7cc" :emissive glow)}]))]))

(defn- chip-node [part glow texture]
  (let [{:keys [x0 x1 z0 z1 turned]} (chip-box part)
        w (- x1 x0) d (- z1 z0) cx (/ (+ x0 x1) 2) cz (/ (+ z0 z1) 2)
        color (or (:color (:spec part)) "#26272b")
        legs (assoc metal :emissive glow)]
    [:group {}
     [:box {:size [w 1.25 d] :position [cx 1.075 cz] :material {:color color :roughness 0.75 :emissive glow}}]
     ;; pin 1 on the far row: the chip is turned round, and so is its print
     [:plane {:size [w d] :rotation [(/ PI -2) 0 (if turned PI 0)] :position [cx 1.704 cz]
              :material {:map texture :roughness 0.75 :emissive glow}}]
     (for [p (:pins part)]
       (let [h (:hole p) out (if (> (:z h) cz) 1 -1)]
         [:group {}
          [:box {:size [0.5 1.2 0.12] :position [(:x h) 0.6 (- (:z h) (* out 0.05))] :material legs}]
          [:box {:size [0.5 0.12 (+ (- (js/Math.abs (- (:z h) cz)) (/ d 2)) 0.1)]
                 :position [(:x h) 1.15 (/ (+ (:z h) cz (* out (/ d 2))) 2)] :material legs}]]))]))

(defn- two-pin-node [part glow bands]
  (let [s (:spec part)
        a (:hole (first (:pins part))) b (:hole (second (:pins part)))
        pa (at a 0) pb (at b 0)
        mid [(/ (+ (:x a) (:x b)) 2) 0 (/ (+ (:z a) (:z b)) 2)]
        span (js/Math.hypot (- (:x b) (:x a)) (- (:z b) (:z a)))
        ux (/ (- (:x b) (:x a)) (max span 0.001)) uz (/ (- (:z b) (:z a)) (max span 0.001))
        lead-mat (assoc metal :emissive glow)
        legs (fn [y] [(lead (at a -0.2) (at a y) 0.07 lead-mat) (lead (at b -0.2) (at b y) 0.07 lead-mat)])
        mid-at (fn [y t] [(+ (nth mid 0) (* ux t)) y (+ (nth mid 2) (* uz t))])
        along (fn [y len] (core/between (mid-at y (/ len -2)) (mid-at y (/ len 2))))]
    (case (:type part)
      ("resistor" "diode")
      (let [y 0.75
            len (min 2.6 (max 1.2 (- span 0.9)))
            {:keys [position quaternion]} (along y len)
            n (count bands)]
        [:group {}
         (legs y)
         (lead (at a y) (at b y) 0.07 lead-mat)
         [:capsule {:radius 0.42 :length (- len 0.84) :position position :quaternion quaternion
                    :material {:color (or (:color s) (if (= "diode" (:type part)) "#2a2a2a" "#d9c49a"))
                               :roughness 0.5 :emissive glow}}]
         ;; the value bands evenly from one end, the tolerance band apart at the other
         (map-indexed (fn [i c]
                        (let [t (cond (= "diode" (:type part)) 0.32
                                      (and (= i (dec n)) (> n 1)) 0.3
                                      :else (+ -0.3 (* i (/ 0.36 (max 1 (- n 2))))))]
                          [:cylinder {:radius 0.445 :height 0.16 :segments 16 :quaternion quaternion
                                      :position (mid-at y (* t len)) :material {:color c :roughness 0.5 :emissive glow}}]))
                      bands)])

      "led"
      (let [y 1.0 color (or (:color s) "#e53935")
            glass {:color color :roughness 0.2 :transparent true :opacity 0.85
                   :emissive (or glow color) :emissive-intensity (if glow 1 (if (:lit s) 0.8 0.15))}]
        [:group {}
         (legs y)
         [:cylinder {:radius 1.15 :height 0.25 :position [(nth mid 0) (+ y 0.12) (nth mid 2)] :material glass}]
         [:cylinder {:radius 1.0 :height 1.4 :position [(nth mid 0) (+ y 0.95) (nth mid 2)] :material glass}]
         [:sphere {:radius 1.0 :theta-length (/ PI 2) :position [(nth mid 0) (+ y 1.65) (nth mid 2)] :material glass}]])

      "capacitor"
      (if (= "ceramic" (:kind s))
        [:group {}
         (legs 0.8)
         [:cylinder {:radius 1.0 :height 0.4 :position [(nth mid 0) 1.4 (nth mid 2)]
                     :quaternion (:quaternion (core/between [0 0 0] [(- uz) 0 ux]))
                     :material {:color (or (:color s) "#d9902f") :emissive glow}}]]
        (let [r (or (:size s) (max 1.0 (+ (/ span 2) 0.3))) h (* r 2.2)]
          [:group {}
           (legs 0.3)
           [:cylinder {:radius r :height h :segments 28 :position [(nth mid 0) (+ 0.3 (/ h 2)) (nth mid 2)]
                       :material {:color (or (:color s) "#24407a") :roughness 0.4 :emissive glow}}]
           ;; the stripe on the minus side
           [:cylinder {:radius (* r 1.01) :height (* h 0.98) :segments 28 :open-ended true
                       :theta-start (- (js/Math.atan2 ux uz) (/ PI 6)) :theta-length (/ PI 3)
                       :position [(nth mid 0) (+ 0.3 (/ h 2)) (nth mid 2)]
                       :material {:color "#cfd3d8" :side "double" :emissive glow}}]
           [:cylinder {:radius (* r 0.98) :height 0.04 :segments 28 :position [(nth mid 0) (+ 0.3 h 0.02) (nth mid 2)]
                       :material (assoc metal :color "#c3c7cc" :emissive glow)}]]))

      "button"
      [:group {}
       (legs 0.3)
       [:box {:size [2.3 1.2 2.3] :position [(nth mid 0) 0.6 (nth mid 2)] :material {:color "#2b2b2b" :emissive glow}}]
       [:cylinder {:radius 0.7 :height 0.6 :position [(nth mid 0) 1.5 (nth mid 2)]
                   :material {:color (or (:color s) "#5a5f66") :emissive glow}}]]

      [:group {}])))

(defn- header-node [part glow state]
  [:group {}
   (map-indexed
    (fn [i p]
      (let [h (:hole p)]
        [:group {}
         [:box {:size [0.98 1.0 0.98] :position [(:x h) 0.5 (:z h)] :material {:color "#1a1a1a" :emissive glow}}]
         [:box {:size [0.25 2.4 0.25] :position [(:x h) 2.0 (:z h)] :material (assoc gold :emissive glow)}]
         ;; every other name a step higher, so neighbours don't overlap
         (when (seq (:name p))
           (label state "top" (:name p) 0.6 [(:x h) (+ 3.7 (* (mod i 2) 0.8)) (:z h)] {}))]))
    (:pins part))])

(defn- part-label [part state]
  (let [s (:spec part)]
    (when (and (:label s) (seq (:pins part)) (not (contains? #{"module" "dip"} (:type part))))
      (let [xs (map #(:x (:hole %)) (:pins part)) zs (map #(:z (:hole %)) (:pins part))]
        (label state "top" (:label s) 0.75
               [(/ (+ (apply min xs) (apply max xs)) 2)
                (case (:type part) "led" 4.2 "capacitor" 4.4 "header" 5.0 2.4)
                (/ (+ (apply min zs) (apply max zs)) 2)]
               {})))))

(defn part-node [part state textures]
  (let [k (:key part)
        glow (get (:glow state) k)]
    [:group {:key (str "part:" k)
             :visible (not (contains? (:hidden state) k))
             :on-click (fn [hit] ((:on-pick state) {:kind "part" :key k :hit hit}))}
     (when (seq (:pins part))
       (case (:type part)
         "module" (module-node part glow (get (:parts textures) k))
         "dip" (when (some #(= 1 (:row %)) (:pins part)) (chip-node part glow (get (:parts textures) k)))
         "header" (header-node part glow state)
         (when (= 2 (count (:pins part)))
           (two-pin-node part glow (get (:bands-of textures) k [])))))
     (part-label part state)]))

;; ── Wires ──

(defn tag-places
  "Where the wires to off-board labels end: one tag per label and board
   side, the tags of a side spread out in a row so they don't overlap.
   key → {:point [x y z] :owner [x y z] (nil but for the first wire of a
   tag) :out [dx 0 dz] (from the tag point to its label) :text}."
  [r]
  (let [b (:board r)
        mid (/ (+ (:z-min b) (:z-max b)) 2)
        side-of (fn [w] (let [s (:side (:to w))]
                          (if (contains? #{"top" "bottom" "left" "right"} s) s (if (< (:z (:hole (:from w))) mid) "top" "bottom"))))
        offs (filter #(:off (:to %)) (:wires r))
        groups (vals (group-by (fn [w] (str (side-of w) "|" (:off (:to w)))) offs))
        tags (map (fn [ws]
                    (let [side (side-of (first ws)) text (:off (:to (first ws)))
                          across (or (= side "top") (= side "bottom"))]
                      {:side side :text text :wires ws
                       :at (/ (reduce + (map #(if across (:x (:hole (:from %))) (:z (:hole (:from %)))) ws)) (count ws))
                       :size (if across (+ 0.3 (* 0.8 (+ 0.8 (* 0.45 (count text))))) 1.3)}))
                  groups)]
    (into {}
          (mapcat
           (fn [[side ts]]
             (let [ts (sort-by :at ts)
                   placed (reduce (fn [acc t]
                                    (let [prev (last acc)
                                          p (if prev (max (:at t) (+ (:pos prev) (/ (+ (:size prev) (:size t)) 2))) (:at t))]
                                      (conj acc (assoc t :pos p))))
                                  [] ts)
                   shift (/ (reduce + (map #(- (:at %) (:pos %)) placed)) (count placed))]
               (mapcat
                (fn [t]
                  (let [pos (+ (:pos t) shift)
                        point (case side
                                "top" [pos 0.4 (- (:z-min b) 3.5)]
                                "bottom" [pos 0.4 (+ (:z-max b) 3.5)]
                                [(if (= side "left") (- (:x-min b) 3.5) (+ (:x-max b) 3.5)) 0.4 pos])
                        half (+ 0.5 (* 0.4 (+ 0.8 (* 0.45 (count (:text t))))))
                        out (case side "top" [0 0 -1.2] "bottom" [0 0 1.2] "left" [(- half) 0 0] [half 0 0])
                        n (count (:wires t))]
                    (map-indexed
                     (fn [i w]
                       (let [o (* 0.55 (- i (/ (dec n) 2)))]
                         [(:key w) {:point (if (or (= side "top") (= side "bottom"))
                                             [(+ (nth point 0) o) 0.4 (nth point 2)]
                                             [(nth point 0) 0.4 (+ (nth point 2) o)])
                                    :owner (when (zero? i) point) :out out :text (:text t)}]))
                     (:wires t))))
                placed)))
           (group-by :side tags)))))

(defn wire-node [r w i places state]
  (let [b (:board r)
        k (:key w)
        s (:spec w)
        under (= "bottom" (:side s))
        y0 (if under (- (:thick b)) 0)
        sg (if under -1 1)
        color (or (:color s) "#2e7d32")
        glow (get (:glow state) k)
        p0 (at (:hole (:from w)) (- y0 (* sg 0.4)))
        tag (get places k)
        p1 (if tag
             (let [p (:point tag)] [(nth p 0) (if under (- y0 0.4) (nth p 1)) (nth p 2)])
             (at (:hole (:to w)) (- y0 (* sg 0.4))))
        span (js/Math.hypot (- (nth p1 0) (nth p0 0)) (- (nth p1 2) (nth p0 2)))
        h (+ (min 5 (+ 1.2 (* span 0.12))) (* (mod i 4) 0.25))
        c1 [(nth p0 0) (+ y0 (* sg h 1.3)) (nth p0 2)]
        c2 [(nth p1 0) (+ y0 (* sg h (if tag 0.6 1.3))) (nth p1 2)]
        midpoint (mapv (fn [j] (/ (+ (nth p0 j) (* 3 (nth c1 j)) (* 3 (nth c2 j)) (nth p1 j)) 8)) [0 1 2])]
    [:group {:key (str "wire:" k)
             :visible (not (contains? (:hidden state) k))
             :on-click (fn [hit] ((:on-pick state) {:kind "wire" :key k :hit hit}))}
     [:tube {:points [p0 c1 c2 p1] :curve "bezier" :radius 0.17 :segments (max 24 (js/Math.round (* span 3)))
             :material {:color color :roughness 0.45 :emissive glow}}]
     (when tag
       [:box {:size [0.55 0.55 0.55] :position p1 :material {:color "#1a1a1a" :emissive glow}}])
     (when-let [o (and tag (:owner tag))]
       (label state "off" (:text tag) 0.8
              [(+ (nth o 0) (nth (:out tag) 0)) (+ (nth p1 1) (* sg 0.4)) (+ (nth o 2) (nth (:out tag) 2))] {}))
     (when (:id w)
       (label state (if under "bottom" "top") (:id w) 0.62
              [(nth midpoint 0) (+ (nth midpoint 1) (* sg 0.55)) (nth midpoint 2)]
              {:fg "#ffffff" :bg color}))]))

;; ── The board ──

(defn board-node [r state textures]
  (let [b (:board r)
        w (- (:x-max b) (:x-min b)) d (- (:z-max b) (:z-min b))
        cx (/ (+ (:x-min b) (:x-max b)) 2) cz (/ (+ (:z-min b) (:z-max b)) 2)
        flipped (boolean (:flipped state))
        see-through {:transparent flipped :opacity (if flipped 0.88 1) :depth-write (not flipped)}]
    [:group {:key "board" :on-click (fn [hit] ((:on-pick state) {:kind "board" :hit hit}))}
     [:box {:size [w (:thick b) d] :position [cx (/ (:thick b) -2) cz]
            :material (merge {:color (if (= "breadboard" (:kind b)) "#e2dccb" (:color b)) :roughness 0.8} see-through)}]
     [:plane {:key "top" :size [w d] :rotation [(/ PI -2) 0 0] :position [cx 0.003 cz]
              :material (merge {:map (:top textures) :roughness 0.85} see-through)}]
     [:plane {:key "bottom" :size [w d] :rotation [(/ PI 2) 0 0] :position [cx (- -0.003 (:thick b)) cz]
              :material (merge {:map (:bottom textures) :roughness 0.85} see-through)}]]))

(defn rings-node
  "Rings on `holes`, on the side of the board that faces the camera."
  [r state]
  (let [y (if (:flipped state) (- -0.02 (:thick (:board r))) 0.02)]
    [:group {:key "rings"}
     (for [h (:rings state)]
       [:ring {:key (:key h) :inner 0.3 :outer 0.48 :rotation [(/ PI -2) 0 0] :position [(:x h) y (:z h)]
               :render-order 3
               :material {:kind "basic" :color "#ffb300" :side "double" :transparent true :opacity 0.95
                          :polygon-offset -2}}])]))

(defn scene
  "Everything on the board, centred on the origin."
  [r state textures]
  (let [b (:board r)
        places (tag-places r)]
    [:group {:key "content" :position [(- (/ (+ (:x-min b) (:x-max b)) 2)) 0 (- (/ (+ (:z-min b) (:z-max b)) 2))]}
     (board-node r state textures)
     (for [p (:parts r)] (part-node p state textures))
     (map-indexed (fn [i w] (wire-node r w i places state)) (:wires r))
     (rings-node r state)
     [:group {:key "extras"} (:extras state)]]))

(def lights
  [[:hemisphere-light {:key "sky" :intensity 2.0}]
   [:directional-light {:key "sun" :intensity 1.6 :position [-20 60 40]}]])
