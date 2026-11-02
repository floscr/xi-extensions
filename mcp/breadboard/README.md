# breadboard: 3D breadboard views as an MCP server

A babashka MCP server whose tools show interactive three.js views (MCP Apps) in
xi, or any MCP Apps host. The views are squint, compiled on demand.

- `breadboard_show` / `breadboard_html`: a breadboard, perfboard or stripboard
  with parts and wires, hole by hole, in the chat or in an HTML document.
- `breadboard_target`: highlight, focus, show/hide or step in an open view.
- `breadboard_eval`: run squint in an open view and get the value back.
- `scene_show`: an arbitrary 3D scene of primitive shapes.

![The breadboard view](docs/breadboard.png)

| Underside (solder side) | Parts on a perfboard | scene_show |
| --- | --- | --- |
| ![](docs/underside.png) | ![](docs/parts.png) | ![](docs/scene.png) |

## Install

Needs [babashka](https://babashka.org); squint, esbuild and
[clj-ui-framework](https://github.com/floscr/clj-ui-framework) (the views'
controls) come as bb dependencies, no node. In `~/.config/xi/mcp.edn`:

```clojure
{:breadboard {:command "bb"
              :args ["--config" "/path/to/xi-extensions/mcp/breadboard/bb.edn" "serve"]
              :code-paths ["/path/to/xi-extensions/mcp/breadboard/bb"
                           "/path/to/xi-extensions/mcp/breadboard/src"]}}
```

Then run `/mcp refresh breadboard`. After changing code, run it again: a view's
URI carries its bundle's hash. A changed `bb/` or `src/` also changes the
server's trust fingerprint, so xi asks once more.

The views load three.js and squint's runtime from jsDelivr, so they need the
internet.

## Layout

```
bb/breadboard_mcp/   the server (bb): rpc (concurrent stdio JSON-RPC), build
                (squint + esbuild, dev server), views (compile on demand,
                import map), channels (agent → open views: commands, long
                poll, eval), breadboard, scene
src/three/      the shared squint lib: core (hiccup → three.js, keyed
                re-render), stage (renderer, camera, picking, framing),
                text, dom, shadow (a shadow root with clj-ui's CSS that
                follows the page theme), host (MCP Apps bridge), live
                (squint eval in page)
src/breadboard/ model.cljc (read by bb and squint), ui/ (the view)
src/scene/ui/   the scene view
dev/            the dev page
```

Bundles import `three` and `squint-cljs` by name. The import map in
`breadboard-mcp.views` resolves them to jsDelivr, the one host a view's CSP allows.
Pages that embed a board get the same import map.

The controls are clj-ui-framework components compiled by squint with the
views (`breadboard-mcp.clj-ui` picks the components and what they require).
Their CSS goes into the bundle, rewritten for a shadow root. A board follows
the nearest `data-theme` of the page; without one, its `theme` attribute
(`light`, `dark` or `system`), else the page's `color-scheme`, else light.

## On a website

`bb bundle` writes `dist/breadboard.js` and `dist/importmap.json`. Load the
import map, then the bundle as a module, and put boards in the page:

```html
<breadboard-view theme="system" sidebar="closed" height="480">
  <script type="application/json">{"board": "mini", "parts": [], "wires": []}</script>
</breadboard-view>
```

The [Xi blog](https://xi-agent.com/blog/) does this for its live boards.

## Tasks

- `bb dev`: http://localhost:9461/ shows the views on fixtures and reloads on
  save. A REPL box evaluates squint in the page; `window.squintEval(code)`
  does the same for a browser driven by an agent.
- `bb test`: the shared model and the server's HTML splicing.
- `bb build`: compiles every view once, to see that they build.
- `bb bundle`: the bundle and import map for a website, in `dist/`.

## Squint notes

- `model.cljc` runs in bb and squint without reader conditionals: no host
  interop, maps and sets aren't called as functions, collections aren't
  compared with `=`.
- A view's CSP has `'unsafe-inline'` but no `'unsafe-eval'`, so live eval
  runs compiled code as an injected inline module script, not `eval`.
- Don't name a local `new`: squint compiles it to a broken `new$` call.
