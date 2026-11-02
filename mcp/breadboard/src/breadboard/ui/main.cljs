(ns breadboard.ui.main
  "The bundle's entry: defines <breadboard-view> (breadboard.ui.element),
   wires up data-bb-* links in the page, and in a chat view
   (globalThis.BREADBOARD_VIEW) connects to the host (breadboard.ui.view).

   Page links: an element with data-bb-highlight / -focus / -only / -show /
   -hide / -view / -step / -note drives a board on click, the one named by
   the nearest data-breadboard (its name attribute), else the first.
   Hovering a data-bb-highlight element lights its targets up while the
   pointer stays."
  (:require [breadboard.ui.element :as element]
            [breadboard.ui.view :as view]
            [three.core :as core]))

(defclass BreadboardView
  (extends js/HTMLElement)
  (constructor [this] (super))
  Object
  (connectedCallback [this] (element/connect! this))
  (setLayout [this layout]
    (set! (.-layout this) layout)
    (if-let [s (element/st this)]
      (do (assoc! s :layout layout) (element/build! this))
      (set! (.-pendingLayout this) layout)))
  (command [this cmd] (element/command! this cmd))
  (add [this hiccup]
    ;; three.core hiccup drawn over the board, e.g. from a live eval
    (let [s (element/st this)]
      (assoc! s :extras (conj (vec (:extras s)) hiccup))
      (element/render! this)))
  (clearExtras [this]
    (assoc! (element/st this) :extras [])
    (element/render! this)))

(def ^:private trigger
  "[data-bb-highlight],[data-bb-focus],[data-bb-only],[data-bb-show],[data-bb-hide],[data-bb-view],[data-bb-step],[data-bb-note]")

(defn- board-for [el]
  (let [named (.closest el "[data-breadboard]")
        boards (vec (js/document.querySelectorAll "breadboard-view"))
        nm (when named (.getAttribute named "data-breadboard"))]
    (or (when nm (some #(when (= nm (.getAttribute % "name")) %) boards))
        (first boards))))

(defn- command-of [el]
  (into {} (keep (fn [k] (when-let [v (.getAttribute el (str "data-bb-" k))]
                           [k (if (= k "step") (js/Number v) v)]))
                 ["highlight" "focus" "only" "show" "hide" "view" "step" "note"])))

(defn- links! []
  (.addEventListener js/document "click"
                     (fn [e]
                       (let [el (when (.-closest (.-target e)) (.closest (.-target e) trigger))
                             board (when el (board-for el))]
                         (when board
                           (when (= "A" (.-tagName el)) (.preventDefault e))
                           (.command board (command-of el))
                           (let [r (.getBoundingClientRect board)]
                             (when (or (neg? (.-bottom r)) (> (.-top r) js/innerHeight))
                               (.scrollIntoView board {:behavior "smooth" :block "center"})))))))
  (let [hovered (atom nil)]
    (.addEventListener js/document "mouseover"
                       (fn [e]
                         (let [el (when (.-closest (.-target e)) (.closest (.-target e) "[data-bb-highlight]"))]
                           (when-not (= el @hovered)
                             (when-let [b (and @hovered (board-for @hovered))] (element/preview! b nil))
                             (reset! hovered el)
                             (when-let [b (and el (board-for el))]
                               (element/preview! b (.getAttribute el "data-bb-highlight")))))))))

(when-not (js/customElements.get "breadboard-view")
  (js/customElements.define "breadboard-view" BreadboardView)
  (set! (.-threeCore js/globalThis) core)
  (links!)
  (when (.-BREADBOARD_VIEW js/globalThis) (view/mount!)))
