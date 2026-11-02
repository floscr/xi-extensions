(ns breadboard-mcp.clj-ui
  "clj-ui-framework (a git dep) for the views: the squint sources of the
   components they use and what those require, and the CSS of just those
   components, rewritten for a shadow root."
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [ui.css.gen :as gen]))

(def components
  "What breadboard.ui.element renders with."
  ["ui.button" "ui.button-group" "ui.badge" "ui.form"])

(defn- ui-dir []
  (some-> (io/resource "ui/css/gen.clj") fs/file fs/parent fs/parent))

(defn- file-of [module ext]
  (fs/path (ui-dir) (str (-> module (str/replace #"^ui\." "") (str/replace "-" "_")) "." ext)))

(defn- requires [module]
  (map second (re-seq #"\[(ui\.[a-z-]+)" (re-find #"(?s)^\(ns .*?\)\)" (slurp (fs/file (file-of module "cljc")))))))

(defn- closure []
  (loop [todo components seen #{}]
    (if-let [m (first todo)]
      (if (seen m) (recur (rest todo) seen) (recur (concat (rest todo) (requires m)) (conj seen m)))
      (sort seen))))

(defn sources
  "module → .cljc file, for breadboard-mcp.views/sources."
  []
  (into {} (map (fn [m] [m (file-of m "cljc")])) (closure)))

(defn- shadow
  "Page-level selectors as the shadow root's: the tokens on :host, the
   theme from the host element's data-theme (breadboard.ui.element mirrors
   the page's)."
  [css]
  (-> css
      (str/replace ":root:not([data-theme=\"light\"])" ":host(:not([data-theme=\"light\"]))")
      (str/replace ":root" ":host")
      (str/replace "[data-theme=\"dark\"]" ":host([data-theme=\"dark\"])")
      (str/replace #"(?m)^(html|body) \{" ":host {")))

(def ^:private css-cache (delay
                           (let [dir (fs/create-temp-dir {:prefix "bb-clj-ui"})]
                             (doseq [m (closure) :let [f (file-of m "css")] :when (fs/exists? f)]
                               (fs/copy f dir))
                             (try
                               (shadow (gen/generate-css (dissoc (gen/read-tokens (io/resource "theme/tokens.edn")) :accents)
                                                         (str dir)))
                               (finally (fs/delete-tree dir))))))

(defn css [] @css-cache)
