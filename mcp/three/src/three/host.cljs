(ns three.host
  "The MCP Apps host (Xi) from inside a view: JSON-RPC over postMessage
   (SEP-1865). `connect!` initializes, applies the host's theme variables and
   hands the tool result's structuredContent to :on-result.")

(defn- post! [msg] (.postMessage js/parent msg "*"))

(defn connect!
  "Opts: :name, :height (px the view asks for), :on-result (fn
   [structured-content]), :on-mode (fn [display-mode]). Returns the host:
   use it with `request`, `call-tool`, `context!`, `message!`."
  [{:keys [name height on-result on-mode]}]
  (let [host {:next 1 :pending (js/Map.)}
        apply-context! (fn [ctx]
                         (let [vars (or (:variables (:styles ctx)) {})]
                           (doseq [k (js/Object.keys vars)]
                             (.setProperty (.. js/document -documentElement -style) k (aget vars k))))
                         (when (and on-mode (:displayMode ctx)) (on-mode (:displayMode ctx))))]
    (.addEventListener js/window "message"
                       (fn [e]
                         (let [m (or (.-data e) {})
                               p (and (:id m) (.get (:pending host) (:id m)))]
                           (cond
                             p (do (.delete (:pending host) (:id m))
                                   (if (:error m) ((:ko p) (:error m)) ((:ok p) (:result m))))
                             (= (:method m) "ui/notifications/tool-result")
                             (when on-result (on-result (or (:structuredContent (:params m)) {})))
                             (= (:method m) "ui/notifications/host-context-changed")
                             (apply-context! (or (:params m) {}))))))
    (-> (js/Promise.
         (fn [ok ko]
           (let [id (:next host)]
             (assoc! host :next (inc id))
             (.set (:pending host) id {:ok ok :ko ko})
             (post! {:jsonrpc "2.0" :id id :method "ui/initialize"
                     :params {:protocolVersion "2026-01-26" :clientInfo {:name name :version "1"}
                              :appCapabilities {:availableDisplayModes ["inline" "fullscreen"]}}}))))
        (.then (fn [r]
                 (apply-context! (or (:hostContext r) {}))
                 (post! {:jsonrpc "2.0" :method "ui/notifications/size-changed"
                         :params {:width (.. js/document -body -clientWidth) :height (or height 480)}})
                 (post! {:jsonrpc "2.0" :method "ui/notifications/initialized" :params {}}))))
    host))

(defn request
  "A JSON-RPC request to the host → Promise of its result."
  [host method params]
  (js/Promise. (fn [ok ko]
                 (let [id (:next host)]
                   (assoc! host :next (inc id))
                   (.set (:pending host) id {:ok ok :ko ko})
                   (post! {:jsonrpc "2.0" :id id :method method :params params})))))

(defn call-tool
  "Call a tool of the view's own extension → Promise of {:content :isError
   :structuredContent}."
  [host tool args]
  (request host "tools/call" {:name tool :arguments args}))

(defn context!
  "Tell the agent something about the view (it reads it with ui_state)."
  [host text]
  (request host "ui/update-model-context" {:content [{:type "text" :text text}]}))

(defn message!
  "Put `text` into the user's message box."
  [host text]
  (request host "ui/message" {:role "user" :content {:type "text" :text text}}))

(defn display-mode!
  "Ask for \"inline\" or \"fullscreen\" → Promise of the mode granted."
  [host mode]
  (.then (request host "ui/request-display-mode" {:mode mode}) (fn [r] (:mode r))))
