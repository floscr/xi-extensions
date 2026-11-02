(ns breadboard-mcp.views
  "The views' JavaScript: the squint under src/ compiled and bundled on
   demand (breadboard-mcp.build: squint + esbuild in bb) and cached until a
   source file changes. A bundle imports three and squint-cljs by bare name;
   `importmap` resolves them to jsDelivr, the one host a view's CSP allows."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [breadboard-mcp.build :as build]))

(def root (fs/parent (fs/parent (fs/parent (fs/absolutize *file*)))))
(def src-dir (fs/path root "src"))

(def three-version "0.170.0")
(def squint-version "0.14.208")

(def importmap
  (str "<script type=\"importmap\">{\"imports\": {"
       "\"three\": \"https://cdn.jsdelivr.net/npm/three@" three-version "/build/three.module.js\", "
       "\"three/addons/\": \"https://cdn.jsdelivr.net/npm/three@" three-version "/examples/jsm/\", "
       "\"squint-cljs\": \"https://cdn.jsdelivr.net/npm/squint-cljs@" squint-version "/index.js\", "
       "\"squint-cljs/\": \"https://cdn.jsdelivr.net/npm/squint-cljs@" squint-version "/\"}}</script>"))

(def csp {:resourceDomains ["https://cdn.jsdelivr.net"]})

(def externals
  "Packages the bundles import by name, for the page's import map."
  ["three" "three/addons/controls/OrbitControls.js"])

(defn- module-name [file]
  (-> (str (fs/relativize src-dir file))
      (str/replace #"\.clj[cs]$" "")
      (str/replace "/" ".")
      (str/replace "_" "-")))

(defn sources
  "module → file for everything under src/ (.cljs, and .cljc shared with the
   server); plus `extra` dirs, named relative to src/'s parent."
  ([] (sources []))
  ([extra]
   (into {} (for [dir (into [src-dir] extra)
                  f (concat (fs/glob dir "**.cljc") (fs/glob dir "**.cljs"))]
              [(if (= dir src-dir) (module-name f)
                   (-> (str (fs/relativize root f)) (str/replace #"\.clj[cs]$" "") (str/replace "/" ".") (str/replace "_" "-")))
               f]))))

(defn- stamp [srcs]
  (hash (sort (map (fn [f] [(str f) (fs/size f) (str (fs/last-modified-time f))]) (vals srcs)))))

(def ^:private cache (atom {}))

(defn bundle
  "The bundled js of entry namespace `entry`, rebuilt when a source changed."
  ([entry] (bundle entry {}))
  ([entry {:keys [extra minify] :or {minify true}}]
   (let [srcs (sources extra)
         k [entry minify]
         s (stamp srcs)
         hit (get @cache k)]
     (if (= s (:stamp hit))
       (:js hit)
       (locking cache
         (let [js (build/compile-bundle {:sources srcs :entry-ns entry
                                            :out-dir (fs/path root ".compiled" (str/replace entry "." "_"))
                                            :external externals :minify minify})]
           (swap! cache assoc k {:stamp s :js js})
           js))))))

(defn version
  "A short hash of `entry`'s bundle, for a view URI that changes with it."
  [entry]
  (format "%08x" (hash (bundle entry))))

(defn module-script
  "An inline module script of `js`, safe inside HTML."
  [js]
  (str "<script type=\"module\">\n" (str/replace js "</script" "<\\/script") "\n</script>"))

(defn view-html
  "An MCP Apps view: the import map, a full-frame `body`, a classic script
   `boot` that runs before the bundle (e.g. sets which entry to mount), and
   the bundle of `entry`."
  [{:keys [title style body boot entry]}]
  (str "<!doctype html>\n<html><head><meta charset=\"utf-8\"><title>" title "</title>\n"
       "<style>html, body { margin: 0; height: 100%; overflow: hidden; background: transparent;\n"
       "  color: var(--color-text-primary, #1d1d1f); font-family: var(--font-sans, system-ui); }\n"
       (or style "") "</style>\n"
       importmap "\n"
       "<script>window.addEventListener('error', e => document.body.insertAdjacentText('beforeend', "
       "'The view failed: ' + (e.message || 'could not load its scripts')));" (or boot "") "</script>\n"
       "</head><body>" (or body "") "\n"
       (module-script (bundle entry)) "\n</body></html>\n"))
