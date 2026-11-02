(ns breadboard-mcp.channels
  "What the agent sends to the views open in a chat, and back. Per chat
   (xi/roomId) and app: a log of recent commands and, per board (\"*\" =
   all), the state they add up to, which a view that opens later starts
   from. Views long-poll `wait`; `eval!` sends squint to a view and waits for
   its `eval-result!`. Sequence numbers are clock based, so a restarted
   server doesn't reuse them.")

(def ^:private channels (atom {}))
(def ^:private waiters (atom {}))
(def ^:private results (atom {}))

(def ^:private state-keys [:view :only :show :hide :labels :highlight :focus :note :step :clear])

(defn- wake! [k]
  (let [[old _] (swap-vals! waiters dissoc k)]
    (doseq [p (get old k)] (deliver p true))))

(defn send!
  "Queue `cmd` for the `app` views of chat `room` (`board` nil = all).
   Returns whether a view polled in the last minutes."
  [app room board cmd]
  (let [k [app room]]
    (swap! channels update k
           (fn [{:keys [n log state] :as ch}]
             (let [n' (max (inc (or n 0)) (System/currentTimeMillis))
                   lasting (select-keys cmd state-keys)]
               (assoc ch
                      :n n'
                      :log (vec (take-last 50 (conj (or log []) {:n n' :board board :cmd cmd})))
                      :state (if (seq lasting)
                               (update (or state {}) (or board "*")
                                       #(if (:clear lasting) (dissoc lasting :clear) (merge % lasting)))
                               state)))))
    (wake! k)
    (some-> (get-in @channels [k :polled-at]) (> (- (System/currentTimeMillis) 150000)))))

(defn- result [n cmds]
  {:content [{:type "text" :text (str (count cmds) " commands")}]
   :structuredContent {:seq n :commands (vec cmds)}})

(defn wait
  "A view's long poll: the commands after `after` for its `board`, or the
   state so far when `after` is 0; answers within `timeout-ms` either way."
  [app room board after timeout-ms]
  (let [k [app room]
        after (or after 0)
        deadline (+ (System/currentTimeMillis) timeout-ms)
        mine? #(or (nil? (:board %)) (= board (:board %)))]
    (swap! channels assoc-in [k :polled-at] (System/currentTimeMillis))
    (if (zero? after)
      (let [{:keys [n state]} (get @channels k)]
        (result (max 1 (or n 0)) (keep identity [(get state "*") (when (seq board) (get state board))])))
      (loop []
        (let [new-for-me #(->> (:log (get @channels k)) (filter (fn [c] (> (:n c) after))) (filter mine?))
              cmds (new-for-me)
              left (- deadline (System/currentTimeMillis))]
          (cond
            (seq cmds) (result (:n (get @channels k)) (map :cmd cmds))
            (<= left 0) (result after [])
            :else (let [p (promise)]
                    (swap! waiters update k (fnil conj #{}) p)
                    ;; a command may have come in between the look and the sign-up
                    (when-not (seq (new-for-me)) (deref p left nil))
                    (recur))))))))

(defn eval!
  "Send squint `code` to the `app` views of chat `room` and wait for the
   first answer: {:value printed} | {:error msg} | nil after `timeout-ms`."
  [app room board code timeout-ms]
  (let [id (str (random-uuid))
        p (promise)]
    (swap! results assoc id p)
    (send! app room board {:eval code :eval-id id})
    (let [r (deref p timeout-ms nil)]
      (swap! results dissoc id)
      r)))

(defn eval-result!
  "A view's answer to `eval!` `id`."
  [id r]
  (some-> (get @results id) (deliver r)))
