(ns three.live
  "Evaluate squint in the running page: the live eval env of views and dev
   pages. The compiler (squint-cljs, ~640 KB) loads through the page's import
   map on first use; the compiled code runs as an inline module script, which
   Xi's view sandbox allows (its CSP has 'unsafe-inline', no 'unsafe-eval').
   Code runs in REPL mode: `def`s land on globalThis.<ns> and, with the
   compiler's state kept between evals, stay reachable by name, like the
   current ns after an (ns …); whatever the page put on globalThis (a view
   puts its board there) is reachable as js/name.")

(def ^:private compiler (atom nil))

(defn- load-compiler []
  (or @compiler (reset! compiler (js* "import('squint-cljs')"))))

(def ^:private waiting (js/Map.))
(def ^:private counter (atom 0))

;; the compiler's state between evals: the namespaces seen, their vars and
;; aliases, and the current one, as compileStringEx hands them back
(def ^:private session (atom {:ns "user" :state {}}))

(defn- printed [v]
  (cond
    (undefined? v) "nil"
    (string? v) (js/JSON.stringify v)
    (fn? v) "#function"
    (instance? js/Element v) (str "#<" (.toLowerCase (.-tagName v)) ">")
    :else (try (let [s (js/JSON.stringify v)] (or s (str v)))
               (catch :default _ (str v)))))

(defn eval-string
  "Compile and run squint `code` → Promise of {:value printed} or
   {:error message}."
  [code]
  (-> (load-compiler)
      (.then (fn [c]
               (let [id (str "e" (swap! counter inc))
                     res (.compileStringEx c (str "(do " code "\n)")
                                           {:repl true :context "return" :elide-exports true :ns (:ns @session)}
                                           (:state @session))
                     _ (reset! session {:ns (str (or (.-ns res) "user")) :state {:ns-state (aget res "ns-state")}})
                     js (.-javascript res)
                     el (js/document.createElement "script")]
                 (js/Promise.
                  (fn [ok _]
                    (.set waiting id ok)
                    (set! (.-type el) "module")
                    (set! (.-textContent el)
                          (str "const done = globalThis.__threeLive.get('" id "');\n"
                               "try { done({ value: await (async function () {\n" js "\n})() }); }\n"
                               "catch (e) { done({ error: String(e && e.message || e) }); }"))
                    (.appendChild js/document.head el)
                    ;; a syntax error never runs the script: give up after a while
                    (js/setTimeout #(ok {:error "no result (did it compile to valid JS?)"}) 20000))))))
      (.then (fn [r]
               (if (:error r)
                 {:error (:error r)}
                 {:value (printed (:value r))})))
      (.catch (fn [e] {:error (str (or (.-message e) e))}))))

(set! (.-__threeLive js/globalThis)
      {:get (fn [id] (fn [r] (when-let [ok (.get waiting id)] (.delete waiting id) (ok r))))})
