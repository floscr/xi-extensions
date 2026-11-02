(ns three.text
  "Text and drawings on canvases, as three.js textures: `label` for a text
   pill (sprites, three.core's :sprite :text), `drawn` for any canvas
   drawing. Labels are cached by text and style; a `drawn` texture is the
   caller's to keep."
  (:require ["three" :as THREE]))

(def font "system-ui, -apple-system, Segoe UI, sans-serif")

(defn texture
  "A three.js texture of `canvas`, in sRGB and sharp at an angle."
  [canvas]
  (let [t (THREE/CanvasTexture. canvas)]
    (set! (.-colorSpace t) THREE/SRGBColorSpace)
    (set! (.-anisotropy t) 8)
    t))

(defn drawn
  "A `w`×`h` px canvas, drawn by (draw! ctx), as a texture."
  [w h draw!]
  (let [c (js/document.createElement "canvas")]
    (set! (.-width c) (js/Math.round w))
    (set! (.-height c) (js/Math.round h))
    (draw! (.getContext c "2d"))
    (texture c)))

(defn pill!
  "Text with a rounded backing, drawn at the origin of `g`'s transform.
   `align` is \"left\", \"right\" or \"center\"; nil `bg` draws no backing."
  [g s size align fg bg]
  (set! (.-font g) (str "600 " size "px " font))
  (let [w (.-width (.measureText g s))
        pad (* size 0.3)
        h (* size 1.25)
        x0 (case align "right" (- 0 w (* pad 2)) "center" (- 0 (/ w 2) pad) 0)]
    (when bg
      (set! (.-fillStyle g) bg)
      (.beginPath g)
      (.roundRect g x0 (/ h -2) (+ w (* pad 2)) h (/ h 2))
      (.fill g))
    (set! (.-fillStyle g) fg)
    (set! (.-textBaseline g) "middle")
    (set! (.-textAlign g) "left")
    (.fillText g s (+ x0 pad) (* size 0.05))))

(def ^:private labels (js/Map.))

(defn label
  "A text pill texture {:texture :aspect}: props :fg (text colour) and :bg
   (backing colour)."
  [s {:keys [fg bg]}]
  (let [fg (or fg "#1d1d1f")
        bg (or bg "#ffffffee")
        k (str s "|" fg "|" bg)]
    (or (.get labels k)
        (let [px 64
              measure (.getContext (js/document.createElement "canvas") "2d")
              _ (set! (.-font measure) (str "600 " px "px " font))
              w (js/Math.ceil (+ (.-width (.measureText measure s)) (* px 0.8)))
              h (js/Math.ceil (* px 1.35))
              v {:texture (drawn w h (fn [g]
                                       (.translate g (/ w 2) (/ h 2))
                                       (pill! g s px "center" fg bg)))
                 :aspect (/ w h)}]
          (.set labels k v)
          v))))
