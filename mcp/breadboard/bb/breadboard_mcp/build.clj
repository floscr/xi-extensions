(ns breadboard-mcp.build
  "Squint in babashka, no node: compile namespaces with squint's compiler,
   bundle them with esbuild (babashka.esbuild), and a dev server that
   rebuilds and live-reloads a page on save (server-sent events).
   `three` and `squint-cljs` stay external imports for the page's import
   map (breadboard-mcp.views/importmap)."
  (:require [babashka.esbuild :as esbuild]
            [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [squint.compiler :as squint]
            [squint.compiler-common :as squint-common]))

;; Squint's JVM emitter renders a regex literal as /pattern/ verbatim: escape
;; its slashes, and move leading inline flags like (?i) to JS flags.
(defmethod squint-common/emit java.util.regex.Pattern [expr _env]
  (let [s (str expr)
        [_ flags body] (re-matches #"(?s)^\(\?([ims]+)\)(.*)" s)
        body (str/replace (or body s) #"(?<!\\)/" "\\\\/")]
    (str \/ body \/ flags)))

(defn- compile-file!
  "Compile one source to <out-dir>/<module>.mjs. Known modules resolve to
   sibling files, `external` packages stay bare imports, anything else is a
   JS global (`Math/max`)."
  [out-dir modules external module file]
  (let [resolve-ns (fn [n]
                     (let [n (str n)]
                       (cond
                         (contains? modules n) (str "./" n ".mjs")
                         (contains? external n) n
                         :else nil)))]
    (spit (fs/file (fs/path out-dir (str module ".mjs")))
          (squint/compile-string (slurp (fs/file file)) {:resolve-ns resolve-ns}))))

(defn compile-bundle
  "Compile `sources` (module name → file) and bundle `entry-ns` into one ES
   module string."
  [{:keys [sources entry-ns out-dir external minify]}]
  (when (fs/exists? out-dir) (fs/delete-tree out-dir))
  (fs/create-dirs out-dir)
  (doseq [[module file] sources]
    (compile-file! out-dir (set (keys sources)) (set external) module file))
  (-> (esbuild/build {:entry-points [(str (fs/path out-dir (str entry-ns ".mjs")))]
                      :bundle true
                      :format :esm
                      :target :es2020
                      :minify (boolean minify)
                      :external (into ["squint-cljs" "squint-cljs/*"] external)})
      :outputs first :contents))

;; ── Dev server ──

(def ^:private reload-snippet
  "<script>(function(){
var es=new EventSource('/__reload');
function overlay(msg){var el=document.getElementById('__err')||document.body.appendChild(Object.assign(document.createElement('pre'),{id:'__err'}));el.style.cssText='position:fixed;inset:0;z-index:99999;margin:0;padding:2rem;background:#300;color:#fbb;font:14px monospace;white-space:pre-wrap;overflow:auto';el.textContent=msg;}
es.onmessage=function(e){var m=JSON.parse(e.data);if(m.type==='error'){overlay(m.message);}else{location.reload();}};
})();</script>")

(def ^:private content-types
  {"html" "text/html" "css" "text/css" "js" "text/javascript" "mjs" "text/javascript"
   "json" "application/json" "svg" "image/svg+xml" "png" "image/png" "jpg" "image/jpeg"})

(defn dev!
  "Serve `app-dir`'s index.html with (build) as its module script
   (/__dev/main.mjs, in place of the page's main.mjs), rebuild when a file
   under `watch-dirs` changes and reload the page. Blocks."
  [{:keys [app-dir port build watch-dirs]}]
  ((requiring-resolve 'babashka.pods/load-pod) 'org.babashka/fswatcher "0.0.7")
  (let [watch! (requiring-resolve 'pod.babashka.fswatcher/watch)
        run-server (requiring-resolve 'org.httpkit.server/run-server)
        as-channel (requiring-resolve 'org.httpkit.server/as-channel)
        send! (requiring-resolve 'org.httpkit.server/send!)
        app-dir (fs/absolutize app-dir)
        state (atom {})
        clients (atom #{})
        broadcast! (fn [msg] (doseq [ch @clients] (send! ch (str "data: " (json/generate-string msg) "\n\n") false)))
        rebuild! (fn []
                   (try (swap! state assoc :bundle (build) :error nil)
                        {:type "reload"}
                        (catch Exception e
                          (let [msg (str (ex-message e) (some->> (:errors (ex-data e)) (map pr-str) (str/join "\n") (str "\n")))]
                            (binding [*out* *err*] (println "Build error:" msg))
                            (swap! state assoc :error msg)
                            {:type "error" :message msg}))))
        pending (atom nil)
        debounced! (fn [] (swap! pending (fn [old] (when old (future-cancel old))
                                           (future (Thread/sleep 50) (broadcast! (rebuild!))))))
        handler
        (fn [{:keys [uri] :as req}]
          (cond
            (= uri "/__reload")
            (as-channel req {:on-open (fn [ch]
                                        (send! ch {:status 200 :headers {"Content-Type" "text/event-stream"
                                                                         "Cache-Control" "no-cache"}} false)
                                        (swap! clients conj ch)
                                        (when-let [err (:error @state)]
                                          (send! ch (str "data: " (json/generate-string {:type "error" :message err}) "\n\n") false)))
                             :on-close (fn [ch _] (swap! clients disj ch))})
            (= uri "/__dev/main.mjs")
            {:status 200 :headers {"Content-Type" "text/javascript" "Cache-Control" "no-cache"} :body (or (:bundle @state) "")}
            (#{"/" "/index.html"} uri)
            {:status 200 :headers {"Content-Type" "text/html"}
             :body (-> (slurp (fs/file app-dir "index.html"))
                       (str/replace #"<script type=\"module\" src=\"[^\"]*\.mjs\"></script>"
                                    "<script type=\"module\" src=\"/__dev/main.mjs\"></script>")
                       (str/replace "</body>" (str reload-snippet "</body>")))}
            :else
            (let [f (fs/normalize (fs/path app-dir (str/replace uri #"^/" "")))]
              (if (and (str/starts-with? (str f) (str app-dir)) (fs/regular-file? f))
                {:status 200 :headers {"Content-Type" (get content-types (fs/extension f) "application/octet-stream")}
                 :body (fs/file f)}
                {:status 404 :body "not found"}))))]
    (rebuild!)
    (doseq [dir watch-dirs]
      (watch! (str (fs/absolutize dir))
              (fn [{:keys [type path]}]
                (when (and (#{:write :create :rename} type)
                           (#{"cljs" "cljc" "html"} (fs/extension path))
                           (not (str/includes? path "/.compiled/")))
                  (debounced!)))
              {:recursive true}))
    (run-server handler {:port port :legacy-return-value? false})
    (println (str "Dev: http://localhost:" port (when (:error @state) " (build error, see the page)")))
    @(promise)))
