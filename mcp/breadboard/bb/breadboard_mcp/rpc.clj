(ns breadboard-mcp.rpc
  "MCP over stdio for babashka, after xi's packages/mcp-bb-example: one
   JSON-RPC message per line, stdout for the protocol only (log to stderr).
   Unlike the example every request runs on its own thread, because some
   tools wait: a view's long poll must not hold up the agent's calls.

   A server is {:name :version :tools (fn [] [tool]) :resources (fn []
   [resource]) :read (fn [uri] resource-or-nil)}. A tool is {:name
   :description :inputSchema :_meta :handler}, the handler (fn [args meta])
   → string | {:content [...] :isError :structuredContent}; `args` has
   keyword keys, `meta` the request's _meta with string keys (\"xi/roomId\",
   \"xi/cwd\")."
  (:require [cheshire.core :as json]
            [clojure.string :as str]))

(def ^:private protocol-version "2024-11-05")

(defn- ->result [x]
  (if (string? x)
    {:content [{:type "text" :text x}] :isError false}
    (merge {:isError false} x)))

(defn handle
  "One decoded request → the reply map, or nil for a notification."
  [{:keys [name version tools resources read]} {:keys [id method params]}]
  (let [reply (fn [result] (when (some? id) {:jsonrpc "2.0" :id id :result result}))
        fail (fn [code msg] (when (some? id) {:jsonrpc "2.0" :id id :error {:code code :message msg}}))]
    (case method
      "initialize" (reply {:protocolVersion (or (:protocolVersion params) protocol-version)
                           :capabilities {:tools {} :resources {}}
                           :serverInfo {:name name :version version}})
      "ping" (reply {})
      "tools/list" (reply {:tools (mapv #(dissoc % :handler) (tools))})
      "resources/list" (reply {:resources (mapv #(dissoc % :text) (resources))})
      "resources/read" (if-let [r (read (:uri params))]
                         (reply {:contents [(select-keys r [:uri :mimeType :text :_meta])]})
                         (fail -32602 (str "Unknown resource: " (:uri params))))
      "tools/call" (if-let [{:keys [handler]} (some #(when (= (:name params) (:name %)) %) (tools))]
                     (reply (try (->result (handler (or (:arguments params) {})
                                                    (update-keys (or (:_meta params) {}) #(subs (str %) 1))))
                                 (catch Exception e
                                   (binding [*out* *err*] (println "tool" (:name params) "failed:" (ex-message e)))
                                   {:content [{:type "text" :text (str (:name params) " failed: " (ex-message e))}]
                                    :isError true})))
                     (fail -32602 (str "Unknown tool: " (:name params))))
      (when-not (str/starts-with? (str method) "notifications/")
        (fail -32601 (str "Method not found: " method))))))

(def ^:private out-lock (Object.))

(defn- send! [msg]
  (let [line (json/generate-string msg)]
    (locking out-lock
      (println line)
      (flush))))

(defn serve!
  "Serve `server` on stdin/stdout until stdin closes; each request on its
   own thread, replies in whatever order they finish."
  [server]
  (doseq [line (line-seq (java.io.BufferedReader. *in*))
          :when (not (str/blank? line))]
    (future
      (let [out (try (handle server (json/parse-string line true))
                     (catch Exception e
                       {:jsonrpc "2.0" :id nil :error {:code -32700 :message (str "Parse error: " (ex-message e))}}))]
        (when out (send! out))))))
