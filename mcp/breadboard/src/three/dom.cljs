(ns three.dom
  "Hiccup to DOM for the overlays around a stage (bars, sidebars, info
   lines), e.g. clj-ui-framework's squint components: [:tag {:class :style
   {..} :on-click f :hidden :title :data-x ...} & children]. Strings become
   text nodes, nils vanish, seqs are flattened, <svg> and what it holds get
   the SVG namespace. Builds fresh elements; no diffing.")

(def ^:private svg-ns "http://www.w3.org/2000/svg")

(defn- iterable? [x]
  (and (some? x) (not (string? x)) (fn? (aget x js/Symbol.iterator))))

(defn- element? [x] (and (vector? x) (string? (first x))))

(defn- build [node svg?]
  (cond
    (nil? node) nil
    (or (string? node) (number? node)) (js/document.createTextNode (str node))
    (element? node)
    (let [[tag & more] node
          props (when (map? (first more)) (first more))
          kids (if props (rest more) more)
          svg? (or svg? (= tag "svg"))
          el (if svg? (js/document.createElementNS svg-ns tag) (js/document.createElement tag))]
      (doseq [k (js/Object.keys (or props {}))]
        (let [v (aget props k)]
          (cond
            (= k "style") (doseq [s (js/Object.keys v)] (.setProperty (.-style el) s (aget v s)))
            (.startsWith k "on-") (.addEventListener el (.slice k 3) v)
            (or (false? v) (nil? v)) nil
            (true? v) (.setAttribute el k "")
            :else (.setAttribute el k v))))
      (doseq [kid (if (iterable? kids) kids [])]
        (doseq [n (if (and (iterable? kid) (not (element? kid))) kid [kid])]
          (when-let [d (build n svg?)] (.appendChild el d))))
      el)
    :else nil))

(defn h [node] (build node false))

(defn replace-children!
  "Put the DOM of `nodes` (a seq of hiccup) into `el`, in place of its children."
  [el nodes]
  (set! (.-textContent el) "")
  (doseq [n nodes] (when-let [d (h n)] (.appendChild el d)))
  el)
