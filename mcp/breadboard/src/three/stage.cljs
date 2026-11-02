(ns three.stage
  "Where a three.core tree is shown: a renderer on a canvas, a scene with a
   :root group to render! into, a camera and orbit controls. It draws on
   demand (`request-render!` when something changed; the controls ask for
   themselves), never in an idle loop. Clicks that weren't drags call the
   :on-click prop of what was hit (three.core/find-prop)."
  (:require ["three" :as THREE]
            ["three/addons/controls/OrbitControls.js" :refer [OrbitControls]]
            [three.core :as core]))

(defn request-render! [stage]
  (when-not (:frame stage)
    (assoc! stage :frame
            (js/requestAnimationFrame
             (fn []
               (assoc! stage :frame nil)
               (.update (:controls stage))
               (.render (:renderer stage) (:scene stage) (:camera stage)))))))

(defn tween!
  "Call (step! k) with k eased from 0 to 1 over `ms`, once per frame, then
   (done!) when given; a zero `ms` steps once to 1."
  ([ms step!] (tween! ms step! nil))
  ([ms step! done!]
   (let [t0 (js/performance.now)]
     ((fn frame [now]
        (let [k (if (pos? ms) (min 1 (/ (- now t0) ms)) 1)
              e (if (< k 0.5) (* 2 k k) (- 1 (/ (js/Math.pow (+ (* -2 k) 2) 2) 2)))]
          (step! e)
          (if (< k 1) (js/requestAnimationFrame frame) (when done! (done!)))))
      t0))))

(defn resize!
  "Fit the drawing buffer and the camera's aspect to `w`×`h` css px."
  [stage w h]
  (when (and (pos? w) (pos? h))
    (.setSize (:renderer stage) w h false)
    (set! (.-aspect (:camera stage)) (/ w h))
    (.updateProjectionMatrix (:camera stage))
    (request-render! stage)))

(defn box-corners [box]
  (let [lo (.-min box) hi (.-max box)]
    (for [x [(.-x lo) (.-x hi)] y [(.-y lo) (.-y hi)] z [(.-z lo) (.-z hi)]]
      (THREE/Vector3. x y z))))

(defn frame!
  "Aim at the middle of `box` (a THREE.Box3) from `direction` and back off
   until all of it is in the picture, `margin` (1.1) to spare."
  ([stage box direction] (frame! stage box direction 1.1))
  ([stage box direction margin]
   (when-not (.isEmpty box)
     (let [camera (:camera stage)
           center (.getCenter box (THREE/Vector3.))
           dir (.normalize (THREE/Vector3. (nth direction 0) (nth direction 1) (nth direction 2)))
           corners (box-corners box)
           place! (fn [dist]
                    (.addScaledVector (.copy (.-position camera) center) dir dist)
                    (.lookAt camera center)
                    (set! (.-near camera) (/ dist 50))
                    (set! (.-far camera) (* dist 6))
                    (.updateProjectionMatrix camera)
                    (.updateMatrixWorld camera true))
           dist (loop [d (* 1.5 (.length (.getSize box (THREE/Vector3.)))) i 0]
                  (if (= i 3)
                    d
                    (do (place! d)
                        (let [spread (apply max (map (fn [c]
                                                       (let [p (.project (.clone c) camera)]
                                                         (max (js/Math.abs (.-x p)) (js/Math.abs (.-y p)))))
                                                     corners))]
                          (recur (* d spread margin) (inc i))))))]
       (place! dist)
       ;; a flight still under way would steer the camera away again
       (assoc! stage :flight (inc (or (:flight stage) 0)))
       (.copy (.-target (:controls stage)) center)
       (.update (:controls stage))
       (request-render! stage)))))

(defn fly-to!
  "Move the camera to look at `target` from `position` (THREE.Vector3s)
   over `ms`."
  [stage target position ms]
  (let [controls (:controls stage) camera (:camera stage)
        ft (.clone (.-target controls)) fp (.clone (.-position camera))
        flight (inc (or (:flight stage) 0))]
    (assoc! stage :flight flight)
    (tween! ms (fn [e]
                 (when (= flight (:flight stage))
                   (.lerpVectors (.-target controls) ft target e)
                   (.lerpVectors (.-position camera) fp position e)
                   (.update controls)
                   (request-render! stage))))))

(defn focus!
  "Fly to `box`, keeping the direction the camera looks from; a box smaller
   than `min-size` is framed as that size."
  [stage box min-size]
  (when-not (.isEmpty box)
    (let [camera (:camera stage) controls (:controls stage)
          c (.getCenter box (THREE/Vector3.))
          size (max min-size (.length (.getSize box (THREE/Vector3.))))
          dir (.normalize (.subVectors (THREE/Vector3.) (.-position camera) (.-target controls)))
          dist (* 1.25 (/ (/ size 2) (js/Math.tan (/ (* (.-fov camera) js/Math.PI) 360))))]
      (fly-to! stage c (.addScaledVector (.clone c) dir dist) 400))))

(defn pick
  "What is under the pointer `event`: {:object :point :distance} of the
   nearest shown object under :root with an :on-click, or nil."
  [stage event]
  (let [canvas (.-domElement (:renderer stage))
        rect (.getBoundingClientRect canvas)
        ptr (THREE/Vector2. (- (* 2 (/ (- (.-clientX event) (.-left rect)) (.-width rect))) 1)
                            (+ (* -2 (/ (- (.-clientY event) (.-top rect)) (.-height rect))) 1))
        ray (THREE/Raycaster.)]
    (.setFromCamera ray ptr (:camera stage))
    (some (fn [hit]
            (let [o (.-object hit)]
              (when (and (core/shown? o) (core/find-prop o :on-click))
                {:object o :point (.-point hit) :distance (.-distance hit)})))
          (.intersectObjects ray (.-children (:root stage)) true))))

(defn create!
  "A stage on `canvas`. Opts: :fov (32), :damping (0.3, the glide after a
   drag), :on-miss (fn [event]) for a click on nothing."
  [canvas opts]
  (let [renderer (THREE/WebGLRenderer. {:canvas canvas :antialias true :alpha true})
        scene (THREE/Scene.)
        camera (THREE/PerspectiveCamera. (or (:fov opts) 32) 1 0.5 2000)
        controls (OrbitControls. camera canvas)
        root (THREE/Group.)
        stage {:renderer renderer :scene scene :camera camera :controls controls :root root :frame nil}
        down (atom nil)]
    (.setPixelRatio renderer (min js/devicePixelRatio 2))
    (set! (.-enableDamping controls) true)
    (set! (.-dampingFactor controls) (or (:damping opts) 0.3))
    (.addEventListener controls "change" #(request-render! stage))
    (.add scene root)
    (.addEventListener canvas "pointerdown" (fn [e] (reset! down [(.-clientX e) (.-clientY e)])))
    (.addEventListener canvas "pointerup"
                       (fn [e]
                         (let [d @down]
                           (when (and d (< (js/Math.hypot (- (.-clientX e) (nth d 0)) (- (.-clientY e) (nth d 1))) 5))
                             (if-let [hit (pick stage e)]
                               ((first (core/find-prop (:object hit) :on-click)) hit)
                               (when-let [miss (:on-miss opts)] (miss e)))))))
    stage))

(defn dispose! [stage]
  (.dispose (:controls stage))
  (.dispose (:renderer stage)))
