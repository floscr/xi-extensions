(ns three.shadow
  "A shadow root for a view's controls: clj-ui-framework's CSS (the
   generated three.clj-ui-css, its tokens on :host) plus the view's own, on
   a host element that follows the page's theme."
  (:require [three.clj-ui-css :as clj-ui-css]
            [three.dom :as dom]))

(defn- page-theme
  "The nearest ancestor's data-theme; else the element's theme attribute
   (light, dark, or system); else the page's color-scheme. nil follows the
   system; a page that says nothing is light."
  [el]
  (or (some-> (.-parentElement el) (.closest "[data-theme]") (.getAttribute "data-theme"))
      (case (.getAttribute el "theme")
        "system" nil
        "light" "light"
        "dark" "dark"
        (let [scheme (.-colorScheme (js/getComputedStyle js/document.documentElement))
              dark? (.includes scheme "dark")]
          (cond (and dark? (.includes scheme "light")) nil
                dark? "dark"
                :else "light")))))

(defn- follow-theme! [el]
  (let [sync! #(if-let [t (page-theme el)]
                 (when-not (= t (.getAttribute el "data-theme")) (.setAttribute el "data-theme" t))
                 (.removeAttribute el "data-theme"))]
    (sync!)
    (.observe (js/MutationObserver. sync!) js/document.documentElement
              {:attributes true :attributeFilter ["data-theme"] :subtree true})))

(defn attach!
  "Open a shadow root on `el` holding the CSS and the DOM of `content`
   (hiccup) → the root."
  [el css content]
  (let [root (.attachShadow el {:mode "open"})]
    (.append root (dom/h [:style (str clj-ui-css/css "\n" css)]) (dom/h content))
    (follow-theme! el)
    root))
