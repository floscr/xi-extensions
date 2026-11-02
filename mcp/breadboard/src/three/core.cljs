(ns three.core
  "Hiccup for three.js, after threeagent: a scene is data, [:tag props &
   children], and `render!` turns it into three.js objects under a parent.
   A re-render reuses the objects of the last one (matched by tag and :key,
   else position) and only rebuilds a geometry when its arguments changed, so
   state changes (visibility, highlight colour) are cheap re-renders.

   Components are plain functions: [f & args] renders (f & args). Tags are
   open: `deftag!` adds one.

   Props every tag takes: :key, :position [x y z], :rotation [x y z] or
   :quaternion [x y z w], :scale (number or [x y z]), :visible,
   :render-order, :on-click (fn [hit]) for three.stage's picking, :user-data.
   Meshes take :material {:color :kind \"standard\"|\"basic\" :roughness
   :metalness :emissive :emissive-intensity :map :transparent :opacity
   :side \"double\" :depth-write :depth-test :polygon-offset}."
  (:require ["three" :as THREE]
            [three.text :as text]))

;; ── Tags ──

(def ^:private tags (js/Map.))

(defn deftag!
  "Register `tag`: {:create (fn [props] object3d) :update (fn [obj props])}."
  [tag spec]
  (.set tags tag spec))

(defn- v3 [xs] (THREE/Vector3. (nth xs 0) (nth xs 1) (nth xs 2)))

(defn between
  "Placement of a cylinder-like (y-up) part from point `a` to `b`:
   {:position :quaternion :length}."
  [a b]
  (let [va (v3 a) vb (v3 b)
        dir (.normalize (.subVectors (THREE/Vector3.) vb va))
        q (.setFromUnitVectors (THREE/Quaternion.) (THREE/Vector3. 0 1 0) dir)]
    {:position [(/ (+ (nth a 0) (nth b 0)) 2) (/ (+ (nth a 1) (nth b 1)) 2) (/ (+ (nth a 2) (nth b 2)) 2)]
     :quaternion [(.-x q) (.-y q) (.-z q) (.-w q)]
     :length (.distanceTo va vb)}))

(defn- transform! [obj props]
  (when-not (:manual-transform props)
    (let [p (or (:position props) [0 0 0])]
      (.set (.-position obj) (nth p 0) (nth p 1) (nth p 2)))
    (if-let [q (:quaternion props)]
      (.set (.-quaternion obj) (nth q 0) (nth q 1) (nth q 2) (nth q 3))
      (let [r (or (:rotation props) [0 0 0])]
        (.set (.-rotation obj) (nth r 0) (nth r 1) (nth r 2))))
    (let [s (:scale props)]
      (cond
        (number? s) (.setScalar (.-scale obj) s)
        s (.set (.-scale obj) (nth s 0) (nth s 1) (nth s 2))
        :else (.setScalar (.-scale obj) 1))))
  (set! (.-visible obj) (not (false? (:visible props))))
  (set! (.-renderOrder obj) (or (:render-order props) 0))
  (set! (.. obj -userData -props) props)
  (when-let [ud (:user-data props)]
    (doseq [k (js/Object.keys ud)] (aset (.-userData obj) k (aget ud k)))))

;; ── Materials ──

(defn- material-kind [spec] (or (:kind spec) "standard"))

(defn- make-material [spec]
  (if (= "basic" (material-kind spec)) (THREE/MeshBasicMaterial.) (THREE/MeshStandardMaterial.)))

(defn- set-material! [m spec]
  (let [basic? (= "basic" (material-kind spec))
        transparent (boolean (:transparent spec))
        tex (or (:map spec) nil)]
    (.set (.-color m) (or (:color spec) "#ffffff"))
    (when-not basic?
      (set! (.-roughness m) (or (:roughness spec) 0.6))
      (set! (.-metalness m) (or (:metalness spec) 0))
      (.set (.-emissive m) (or (:emissive spec) "#000000"))
      (set! (.-emissiveIntensity m) (or (:emissive-intensity spec) 1)))
    (when (or (not= tex (.-map m)) (not= transparent (.-transparent m)))
      (set! (.-needsUpdate m) true))
    (set! (.-map m) tex)
    (set! (.-transparent m) transparent)
    (set! (.-opacity m) (or (:opacity spec) 1))
    (set! (.-side m) (if (= "double" (:side spec)) THREE/DoubleSide THREE/FrontSide))
    (set! (.-depthWrite m) (not (false? (:depth-write spec))))
    (set! (.-depthTest m) (not (false? (:depth-test spec))))
    (set! (.-polygonOffset m) (boolean (:polygon-offset spec)))
    (set! (.-polygonOffsetFactor m) (or (:polygon-offset spec) 0))
    m))

;; ── Meshes: a geometry from props, rebuilt only when its arguments change ──

(defn- curve [points kind]
  (let [vs (mapv v3 points)]
    (if (= kind "bezier")
      (THREE/CubicBezierCurve3. (nth vs 0) (nth vs 1) (nth vs 2) (nth vs 3))
      (THREE/CatmullRomCurve3. vs))))

(def ^:private geometries
  {"box"      [[:size]
               (fn [p] (let [s (or (:size p) [1 1 1])] (THREE/BoxGeometry. (nth s 0) (nth s 1) (nth s 2))))]
   "plane"    [[:size]
               (fn [p] (let [s (or (:size p) [1 1])] (THREE/PlaneGeometry. (nth s 0) (nth s 1))))]
   "sphere"   [[:radius :segments :theta-length]
               (fn [p] (THREE/SphereGeometry. (or (:radius p) 0.5) (or (:segments p) 24) (/ (or (:segments p) 24) 2)
                                              0 (* 2 js/Math.PI) 0 (or (:theta-length p) js/Math.PI)))]
   "cylinder" [[:radius :radius-top :radius-bottom :height :segments :open-ended :theta-start :theta-length]
               (fn [p] (THREE/CylinderGeometry. (or (:radius-top p) (:radius p) 0.5) (or (:radius-bottom p) (:radius p) 0.5)
                                                (or (:height p) 1) (or (:segments p) 24) 1 (boolean (:open-ended p))
                                                (or (:theta-start p) 0) (or (:theta-length p) (* 2 js/Math.PI))))]
   "cone"     [[:radius :height :segments]
               (fn [p] (THREE/ConeGeometry. (or (:radius p) 0.5) (or (:height p) 1) (or (:segments p) 24)))]
   "capsule"  [[:radius :length]
               (fn [p] (THREE/CapsuleGeometry. (or (:radius p) 0.5) (or (:length p) 1) 6 16))]
   "ring"     [[:inner :outer :segments]
               (fn [p] (THREE/RingGeometry. (or (:inner p) 0.3) (or (:outer p) 0.5) (or (:segments p) 24)))]
   "torus"    [[:radius :tube]
               (fn [p] (THREE/TorusGeometry. (or (:radius p) 0.5) (or (:tube p) 0.15) 16 48))]
   "torus-knot" [[:radius :tube]
                 (fn [p] (THREE/TorusKnotGeometry. (or (:radius p) 0.35) (or (:tube p) 0.1) 96 12))]
   "tube"     [[:points :curve :radius :segments]
               (fn [p] (THREE/TubeGeometry. (curve (:points p) (:curve p)) (or (:segments p) 32) (or (:radius p) 0.1) 8))]})

(defn- geometry-key [tag props]
  (js/JSON.stringify (mapv #(get props %) (first (get geometries tag)))))

(defn- create-mesh [tag props]
  (let [geom (if (= tag "mesh") (:geometry props) ((second (get geometries tag)) props))
        spec (or (:material props) {})
        mesh (THREE/Mesh. geom (set-material! (make-material spec) spec))]
    (set! (.. mesh -userData -geometryKey) (if (= tag "mesh") nil (geometry-key tag props)))
    mesh))

(defn- update-mesh [mesh tag props]
  (if (= tag "mesh")
    (when-not (= (.-geometry mesh) (:geometry props)) (set! (.-geometry mesh) (:geometry props)))
    (let [k (geometry-key tag props)]
      (when-not (= k (.. mesh -userData -geometryKey))
        (.dispose (.-geometry mesh))
        (set! (.-geometry mesh) ((second (get geometries tag)) props))
        (set! (.. mesh -userData -geometryKey) k))))
  (let [spec (or (:material props) {})]
    (if (= (material-kind spec) (.. mesh -userData -materialKind))
      (set-material! (.-material mesh) spec)
      (do (.dispose (.-material mesh))
          (set! (.-material mesh) (set-material! (make-material spec) spec)))))
  (set! (.. mesh -userData -materialKind) (material-kind (or (:material props) {}))))

(doseq [tag (conj (vec (keys geometries)) "mesh")]
  (deftag! tag {:create (fn [props] (let [m (create-mesh tag props)]
                                      (set! (.. m -userData -materialKind) (material-kind (or (:material props) {})))
                                      m))
                :update (fn [obj props] (update-mesh obj tag props))}))

;; ── Other tags ──

(deftag! "group" {:create (fn [_] (THREE/Group.))})

;; an object made elsewhere, put into the tree as is
(deftag! "object" {:create (fn [props] (let [o (:object props)] (set! (.. o -userData -external) true) o))})

(defn- sprite-texture [props]
  (or (:texture props) (:texture (text/label (or (:text props) "") props))))

(deftag! "sprite"
  {:create (fn [props]
             (THREE/Sprite. (THREE/SpriteMaterial. {:map (sprite-texture props) :transparent true
                                                    :depthTest (not (false? (:depth-test props)))})))
   :update (fn [obj props]
             (let [{:keys [texture aspect]} (if (:texture props)
                                              {:texture (:texture props) :aspect (or (:aspect props) 1)}
                                              (text/label (or (:text props) "") props))
                   h (or (:size props) 1)]
               (when-not (= texture (.. obj -material -map))
                 (set! (.. obj -material -map) texture)
                 (set! (.. obj -material -needsUpdate) true))
               (.set (.-scale obj) (* h aspect) h 1)))})

(deftag! "hemisphere-light"
  {:create (fn [_] (THREE/HemisphereLight.))
   :update (fn [obj props]
             (.set (.-color obj) (or (:sky props) "#ffffff"))
             (.set (.-groundColor obj) (or (:ground props) "#8a8a8a"))
             (set! (.-intensity obj) (or (:intensity props) 1)))})

(deftag! "directional-light"
  {:create (fn [_] (THREE/DirectionalLight.))
   :update (fn [obj props]
             (.set (.-color obj) (or (:color props) "#ffffff"))
             (set! (.-intensity obj) (or (:intensity props) 1)))})

(deftag! "ambient-light"
  {:create (fn [_] (THREE/AmbientLight.))
   :update (fn [obj props]
             (.set (.-color obj) (or (:color props) "#ffffff"))
             (set! (.-intensity obj) (or (:intensity props) 1)))})

(deftag! "grid"
  {:create (fn [props]
             (let [g (THREE/GridHelper. (or (:size props) 20) (or (:divisions props) 20)
                                        (or (:center-color props) "#888888") (or (:color props) "#444444"))]
               (set! (.. g -material -transparent) true)
               (set! (.. g -material -opacity) (or (:opacity props) 0.35))
               g))})

;; ── Rendering ──

(defn- element? [x]
  (and (vector? x) (let [h (first x)] (or (string? h) (fn? h)))))

(defn- iterable? [x]
  (and (some? x) (not (string? x)) (fn? (aget x js/Symbol.iterator))))

(defn expand
  "The element vectors a node stands for: components called, nils dropped,
   seqs flattened."
  [node]
  (cond
    (element? node) (if (fn? (first node))
                      (expand (apply (first node) (rest node)))
                      [node])
    (iterable? node) (vec (mapcat expand node))
    :else []))

(defn- parse [el]
  (let [p (second el)
        props? (map? p)]
    [(first el) (if props? p {}) (if props? (drop 2 el) (drop 1 el))]))

(defn dispose!
  "Free what `obj` and its children own: geometries and materials (textures
   are shared and stay)."
  [obj]
  (.traverse obj (fn [o]
                   (when-not (.. o -userData -external)
                     (when (.-geometry o) (.dispose (.-geometry o)))
                     (when (.-material o) (.dispose (.-material o)))))))

(defn- create [tag props]
  (let [spec (.get tags tag)]
    (when-not spec (throw (js/Error. (str "three.core: no tag " tag))))
    (let [obj ((:create spec) props)]
      (set! (.. obj -userData -tag) tag)
      obj)))

(defn render!
  "Make `parent`'s children match `nodes` (an element, a seq of them, or
   components), reusing the objects of the last render."
  [parent nodes]
  (let [old (or (.. parent -userData -kids) (js/Map.))
        next (js/Map.)]
    (doseq [[i el] (map-indexed vector (expand nodes))]
      (let [[tag props kids] (parse el)
            k (str tag "|" (if (some? (:key props)) (:key props) (str "#" i)))
            prev (.get old k)
            obj (or prev (create tag props))]
        (.delete old k)
        ;; the tag's update after the transform, so it can size (a sprite's aspect)
        (transform! obj props)
        (when-let [upd (:update (.get tags tag))] (upd obj props))
        (when-not (= (.-parent obj) parent) (.add parent obj))
        (.set next k obj)
        (render! obj kids)))
    (doseq [o (vec (.values old))]
      (.remove parent o)
      (dispose! o))
    (set! (.. parent -userData -kids) next)
    parent))

(defn find-prop
  "The first `k` prop on `obj` or its ancestors, with the object holding it:
   [value object], or nil."
  [obj k]
  (loop [o obj]
    (when o
      (let [v (get (.. o -userData -props) k)]
        (if (some? v) [v o] (recur (.-parent o)))))))

(defn shown?
  "Is `obj` visible, its ancestors too?"
  [obj]
  (loop [o obj]
    (cond (nil? o) true
          (not (.-visible o)) false
          :else (recur (.-parent o)))))
