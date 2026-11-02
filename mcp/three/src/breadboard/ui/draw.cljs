(ns breadboard.ui.draw
  "The flat drawings of a board, as textures: both faces of the board
   (holes, strips, rails, row and column labels; the solder side with every
   pin dotted and named), a module's top and a chip's print."
  (:require [breadboard.model :as model]
            [three.text :as text]))

(defn- arc! [g x y r color]
  (set! (.-fillStyle g) color)
  (.beginPath g)
  (.arc g x y r 0 (* 2 js/Math.PI))
  (.fill g))

(defn- label! [g s x y size color]
  (set! (.-font g) (str "600 " size "px " text/font))
  (set! (.-fillStyle g) color)
  (set! (.-textAlign g) "center")
  (set! (.-textBaseline g) "middle")
  (.fillText g s x y))

(defn pin-name!
  "A pin name along a row: turned to read upwards, reaching towards
   canvas-y `towards` so it stays inside a module's outline."
  [g s x y towards size fg bg]
  (let [dir (if (> towards y) 1 -1)]
    (.save g)
    (.translate g x (+ y (* dir size 0.85)))
    (.rotate g (/ js/Math.PI -2))
    (text/pill! g s size (if (pos? dir) "right" "left") fg bg)
    (.restore g)))

(defn board-face
  "One face of the board as a texture; `bottom?` draws the underside,
   mirrored front to back, so a hole sits where it is on top."
  [r bottom?]
  (let [b (:board r)
        bb? (= "breadboard" (:kind b))
        P (max 16 (min 48 (js/Math.floor (/ 4096 (- (:x-max b) (:x-min b))))))
        W (* (- (:x-max b) (:x-min b)) P)
        H (* (- (:z-max b) (:z-min b)) P)
        X (fn [x] (* (- x (:x-min b)) P))
        Z (fn [z] (* (if bottom? (- (:z-max b) z) (- z (:z-min b))) P))
        band (fn [z0 z1] [(min (Z z0) (Z z1)) (js/Math.abs (- (Z z1) (Z z0)))])
        ink (if bb? "#8d8572" "#ffffffb0")
        holes (model/holes b)]
    (text/drawn W H
      (fn [g]
        (set! (.-fillStyle g) (if (and bb? bottom?) "#ebe6d6" (:color b)))
        (.fillRect g 0 0 W H)
        (if bb?
          (do
            (when-not bottom?
              (let [[y h] (band 8.05 8.95)] (set! (.-fillStyle g) "#00000012") (.fillRect g 0 y W h)))
            (doseq [[rail z color] [["T-" -0.65 "#1e88e5"] ["T+" 1.65 "#e53935"] ["B-" 15.35 "#1e88e5"] ["B+" 17.65 "#e53935"]]]
              (when (some? (get (:rails b) rail))
                (let [[y h] (band (- z 0.05) (+ z 0.05))
                      sign (if (= "+" (subs rail 1)) "+" "−")]
                  (set! (.-fillStyle g) color)
                  (.fillRect g (X -0.8) y (- (X (- (:cols b) 0.2)) (X -0.8)) h)
                  (label! g sign (X -1.4) (Z (get (:rails b) rail)) (* 0.75 P) color)
                  (label! g sign (X (+ (:cols b) 0.4)) (Z (get (:rails b) rail)) (* 0.75 P) color))))
            (when bottom?
              ;; the metal clips inside: one per 5-hole strip, one per rail
              (set! (.-fillStyle g) "#c4c8cd")
              (set! (.-strokeStyle g) "#9ba1a8")
              (set! (.-lineWidth g) (* P 0.04))
              (let [bar! (fn [x0 x1 z0 z1]
                           (let [[y h] (band z0 z1)]
                             (.beginPath g)
                             (.roundRect g (X x0) y (- (X x1) (X x0)) h (* P 0.2))
                             (.fill g) (.stroke g)))]
                (doseq [col (range 1 (inc (:cols b)))]
                  (bar! (- col 1.28) (- col 0.72) 2.6 7.4)
                  (bar! (- col 1.28) (- col 0.72) 9.6 14.4))
                (doseq [z (vals (:rails b))]
                  (bar! -0.4 (- (:cols b) 0.6) (- z 0.28) (+ z 0.28)))))
            (doseq [h holes]
              (if bottom?
                (arc! g (X (:x h)) (Z (:z h)) (* 0.1 P) "#6d727a")
                (do (set! (.-fillStyle g) "#3b3832")
                    (.fillRect g (- (X (:x h)) (* P 0.19)) (- (Z (:z h)) (* P 0.19)) (* P 0.38) (* P 0.38)))))
            (doseq [col (range 1 (inc (:cols b)))]
              (when (or (= col 1) (<= (:cols b) 30) (zero? (mod col 5)))
                (label! g (str col) (X (dec col)) (Z 2.05) (* 0.42 P) ink)
                (label! g (str col) (X (dec col)) (Z 15.0) (* 0.42 P) ink)))
            (doseq [row (:row-order b)]
              (label! g row (X -1.1) (Z (get (:rows b) row)) (* 0.48 P) ink)
              (label! g row (X (+ (:cols b) 0.1)) (Z (get (:rows b) row)) (* 0.48 P) ink)))
          (do
            (when (and bottom? (= "stripboard" (:kind b)))
              (set! (.-fillStyle g) "#c99a52")
              (doseq [z (vals (:rows b))]
                (let [[y h] (band (- z 0.4) (+ z 0.4))]
                  (.fillRect g (X -0.5) y (- (X (- (:cols b) 0.5)) (X -0.5)) h))))
            (doseq [h holes]
              (arc! g (X (:x h)) (Z (:z h)) (* 0.34 P) "#c99a52")
              (arc! g (X (:x h)) (Z (:z h)) (* 0.13 P) "#1b1b1b"))
            (doseq [col (range 1 (inc (:cols b)))]
              (when (or (= col 1) (<= (:cols b) 30) (zero? (mod col 5)))
                (label! g (str col) (X (dec col)) (Z -1.1) (* 0.42 P) ink)
                (label! g (str col) (X (dec col)) (Z (- (:z-max b) 0.9)) (* 0.42 P) ink)))
            (doseq [row (:row-order b)]
              (label! g row (X -1.3) (Z (get (:rows b) row)) (* 0.48 P) ink)
              (label! g row (X (+ (:cols b) 0.3)) (Z (get (:rows b) row)) (* 0.48 P) ink))))
        (when bottom?
          ;; the solder side: every pin a dot, a module's or chip's pins named
          (doseq [part (:parts r)]
            (when (seq (:pins part))
              (let [cz (/ (reduce + (map #(:z (:hole %)) (:pins part))) (count (:pins part)))]
                (doseq [pin (:pins part)]
                  (let [h (:hole pin)]
                    (arc! g (X (:x h)) (Z (:z h)) (* 0.3 P) "#f2c94c")
                    (arc! g (X (:x h)) (Z (:z h)) (* 0.1 P) "#7a5d12")
                    (when (and (seq (:name pin)) (contains? #{"module" "dip" "header"} (:type part)))
                      (pin-name! g (:name pin) (X (:x h)) (Z (:z h)) (Z cz) (* P 0.36) "#ffffff" "#202124d9"))))))))))))

(defn module-top
  "A module's top: its colour, gold pads with the pin names, the marks at
   its edges and its label."
  [part box color]
  (let [P 48
        W (* (- (:x1 box) (:x0 box)) P)
        H (* (- (:z1 box) (:z0 box)) P)
        X (fn [x] (* (- x (:x0 box)) P))
        Z (fn [z] (* (- z (:z0 box)) P))
        cz (/ (+ (:z0 box) (:z1 box)) 2)]
    (text/drawn W H
      (fn [g]
        (set! (.-fillStyle g) color)
        (.fillRect g 0 0 W H)
        (set! (.-strokeStyle g) "#ffffff30")
        (set! (.-lineWidth g) (* P 0.08))
        (.strokeRect g (* P 0.08) (* P 0.08) (- W (* P 0.16)) (- H (* P 0.16)))
        (doseq [pin (:pins part)]
          (let [h (:hole pin)]
            (arc! g (X (:x h)) (Z (:z h)) (* P 0.3) "#d4af37")
            (arc! g (X (:x h)) (Z (:z h)) (* P 0.12) "#5a4510")
            (when (seq (:name pin))
              (pin-name! g (:name pin) (X (:x h)) (Z (:z h)) (Z cz) (* P 0.36) "#ffffffee" nil))))
        (doseq [m (or (:marks (:spec part)) [])]
          (let [side (or (:side m) "left")]
            (.save g)
            (case side
              "left" (do (.translate g (* P 0.6) (/ H 2)) (.rotate g (/ js/Math.PI 2)))
              "right" (do (.translate g (- W (* P 0.6)) (/ H 2)) (.rotate g (/ js/Math.PI -2)))
              "top" (.translate g (/ W 2) (* P 0.6))
              (.translate g (/ W 2) (- H (* P 0.6))))
            (text/pill! g (or (:text m) "") (* P 0.42) "center" "#202124" "#ffffffcc")
            (.restore g)))
        (when (seq (:label part))
          (let [size (min (* P 1.1) (/ (* W 0.8) (max 1 (* (count (:label part)) 0.58))) (* H 0.3))]
            (label! g (:label part) (/ W 2) (/ H 2) size "#ffffffe8")))))))

(defn chip-top
  "A chip's print: the notch at pin 1's end, the pin-1 dot and its label."
  [part box dir]
  (let [P 48
        W (* (- (:x1 box) (:x0 box)) P)
        H (* (- (:z1 box) (:z0 box)) P)
        pin1 (:hole (first (:pins part)))]
    (text/drawn W H
      (fn [g]
        (set! (.-fillStyle g) (or (:color (:spec part)) "#26272b"))
        (.fillRect g 0 0 W H)
        (arc! g (if (pos? dir) 0 W) (/ H 2) (* P 0.42) "#121214")
        (arc! g (+ (* (- (:x pin1) (:x0 box)) P) (* dir P 0.05)) (- H (* P 0.55)) (* P 0.18) "#5c5e66")
        (when (seq (:label part))
          (let [size (min (* P 0.8) (/ (* W 0.8) (max 1 (* (count (:label part)) 0.6))))]
            (label! g (:label part) (/ W 2) (/ H 2) size "#e8e8e8")))))))

(def ^:private band-colors ["#111111" "#7b4a24" "#d32f2f" "#f57c00" "#fbc02d" "#388e3c" "#1e63c6" "#7b1fa2" "#8a8a8a" "#f5f5f5"])
(def ^:private named-bands {"black" 0 "brown" 1 "red" 2 "orange" 3 "yellow" 4 "green" 5 "blue" 6 "violet" 7 "purple" 7 "grey" 8 "gray" 8 "white" 9})

(defn resistor-bands
  "The colour bands of a resistor spec: its :bands (names or CSS colours) or
   the 4 bands of its :ohms."
  [spec]
  (cond
    (vector? (:bands spec))
    (mapv (fn [b] (cond (some? (get named-bands b)) (nth band-colors (get named-bands b))
                        (= b "gold") "#c9a227" (= b "silver") "#b0b0b0" :else b))
          (:bands spec))
    (pos? (or (:ohms spec) 0))
    (let [v (:ohms spec)
          e0 (- (js/Math.floor (js/Math.log10 v)) 1)
          d0 (js/Math.round (/ v (js/Math.pow 10 e0)))
          [d e] (if (>= d0 100) [(js/Math.round (/ d0 10)) (inc e0)] [d0 e0])
          mult (cond (>= e 0) (nth band-colors e) (= e -1) "#c9a227" :else "#b0b0b0")]
      [(nth band-colors (js/Math.floor (/ d 10))) (nth band-colors (mod d 10)) mult "#c9a227"])
    :else []))
