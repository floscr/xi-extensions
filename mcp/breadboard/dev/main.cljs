(ns dev.main
  "The dev page's entry: the breadboard element and a squint REPL into the
   page (three.live), also as window.squintEval for an agent's browser."
  (:require [breadboard.ui.main]
            [three.live :as live]))

(set! (.-squintEval js/globalThis) live/eval-string)
(set! (.-board js/globalThis) (js/document.querySelector "breadboard-view"))

(let [code (js/document.getElementById "code")
      out (js/document.getElementById "out")
      run! (fn []
             (set! (.-textContent out) "…")
             (.then (live/eval-string (.-value code))
                    (fn [r] (set! (.-textContent out) (or (:error r) (:value r))))))]
  (.addEventListener (js/document.getElementById "run") "click" run!)
  (.addEventListener code "keydown" (fn [e] (when (and (= "Enter" (.-key e)) (or (.-metaKey e) (.-ctrlKey e)))
                                              (.preventDefault e)
                                              (run!)))))
