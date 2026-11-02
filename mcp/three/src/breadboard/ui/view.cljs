(ns breadboard.ui.view
  "The breadboard view in a chat (MCP Apps): the <breadboard-view> filling
   the frame, its layout from the tool result. Clicks and tutorial steps go
   to the agent as context; the agent's breadboard_target commands and
   breadboard_eval code arrive through a long poll of breadboard_wait, and
   an eval's value goes back with breadboard_eval_result."
  (:require [three.host :as host]
            [three.live :as live]))

(defn- sleep [ms] (js/Promise. (fn [ok _] (js/setTimeout ok ms))))

(defn- run-eval! [h board {:keys [eval eval-id]}]
  (set! (.-board js/globalThis) board)
  (-> (live/eval-string eval)
      (.then (fn [r] (host/call-tool h "breadboard_eval_result"
                                     (if (:error r) {:id eval-id :error (:error r)} {:id eval-id :value (:value r)}))))))

(defn ^:async listen!
  "Apply breadboard_target's commands as they come: the state so far first,
   then each new one; gives up after a few failures in a row."
  [h board]
  (loop [after 0 fails 0]
    (when (< fails 6)
      (let [r (js-await (-> (host/call-tool h "breadboard_wait" {:board (or (:id (.-layout board)) "") :after after})
                            (.catch (fn [_] nil))))]
        (if (or (nil? r) (:isError r))
          (do (js-await (sleep (* 2000 (inc fails))))
              (recur after (inc fails)))
          (let [sc (or (:structuredContent r) {})]
            (doseq [c (or (:commands sc) [])]
              (if (:eval c) (run-eval! h board c) (.command board c)))
            (recur (or (:seq sc) after) 0)))))))

(defn mount!
  "Run the <breadboard-view> in this page as the chat's view."
  []
  (let [board (js/document.querySelector "breadboard-view")
        mode (atom "inline")
        listening (atom false)
        ;; the result comes after connect! returned: reach the host through this
        host-ref (atom nil)
        h (host/connect! {:name "breadboard" :height 480
                          :on-mode (fn [m] (reset! mode m))
                          :on-result (fn [sc]
                                       (.setLayout board (or (:layout sc) {}))
                                       (when-not @listening
                                         (reset! listening true)
                                         (listen! @host-ref board)))})]
    (reset! host-ref h)
    (set! (.-board js/globalThis) board)
    (set! (.-onFullscreen board)
          (fn [] (.then (host/display-mode! h (if (= @mode "fullscreen") "inline" "fullscreen"))
                        (fn [m] (reset! mode m)))))
    (.addEventListener board "bb-select"
                       (fn [e] (when (seq (:text (.-detail e)))
                                 (host/context! h (str "In the breadboard view the user selected: " (:text (.-detail e)))))))
    (.addEventListener board "bb-step"
                       (fn [e] (let [d (.-detail e)]
                                 (host/context! h (str "The user is on tutorial step " (:step d) " of " (:of d) ": " (:title d)
                                                       (when (seq (:text d)) (str " (" (:text d) ")")))))))))
