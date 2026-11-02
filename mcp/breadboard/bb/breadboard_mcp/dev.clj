(ns breadboard-mcp.dev
  "`bb dev`: the views on fixtures (dev/index.html), rebuilt and reloaded on
   every save (breadboard-mcp.build/dev!), with a squint REPL into the page:
   the box under the boards, or window.squintEval(code) → Promise of
   {value|error} for a browser driven by an agent."
  (:require [babashka.fs :as fs]
            [breadboard-mcp.build :as build]
            [breadboard-mcp.views :as views]))

(def port 9461)

(defn -main [& _]
  (build/dev! {:app-dir (str (fs/path views/root "dev"))
                  :port port
                  :build #(views/bundle "dev.main" {:extra [(fs/path views/root "dev")] :minify false})
                  :watch-dirs [(str views/src-dir) (str (fs/path views/root "dev"))]}))
