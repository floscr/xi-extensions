;; A breadboard layout as data: holes, parts with their pins, wires and what
;; they connect (nets), and what is wrong with it. One model for both halves:
;; the MCP server (bb) validates layouts with `problems`, the view (squint,
;; breadboard/ui) draws `resolve-layout`'s result. So the code here runs in
;; both without reader conditionals: no host interop, no `name`/`parse-long`,
;; maps and sets are never called as functions, collections are never
;; compared with `=` (squint compares them by identity).
;;
;; One unit is one 0.1 in pitch. Column c sits at x = c - 1; z grows towards
;; the viewer, so row a and the B rails are the near edge seen from above.
(ns breadboard.model
  (:require [clojure.string :as str]))

(def ^:private bb-rows {"j" 3 "i" 4 "h" 5 "g" 6 "f" 7 "e" 10 "d" 11 "c" 12 "b" 13 "a" 14})
(def ^:private bb-row-order ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j"])
(def ^:private across {"a" "j" "b" "i" "c" "h" "d" "g" "e" "f" "f" "e" "g" "d" "h" "c" "i" "b" "j" "a"})
(def ^:private rails {"T-" 0 "T+" 1 "B-" 16 "B+" 17})
(def ^:private sizes {"full" 63 "half" 30 "mini" 17})
(def ^:private alphabet "abcdefghijklmnopqrstuvwxyz")

(defn- q [x] (str "\"" x "\""))

;; a keyword's name: (str :k) is ":k" in SCI, "k" in squint
(defn- kname [k] (let [s (str k)] (if (str/starts-with? s ":") (subs s 1) s)))

(defn- digits [s]
  (reduce (fn [n c] (+ (* n 10) (str/index-of "0123456789" c))) 0 s))

(defn letters [i]
  (if (< i 26)
    (subs alphabet i (inc i))
    (str (letters (dec (quot i 26))) (subs alphabet (mod i 26) (inc (mod i 26))))))

;; ── Boards ──

(defn board
  "The board of a layout's :board: a size name or {:type :cols :rows :color
   :split_rails}. :error when it is none of the known ones."
  [spec]
  (let [b (if (string? spec) {:type spec} (or spec {}))
        type (or (:type b) "half")]
    (cond
      (or (= type "perfboard") (= type "stripboard"))
      (let [cols (or (:cols b) 30) n (or (:rows b) 20)
            order (mapv letters (range n))]
        {:kind type :cols cols :row-order order
         :rows (into {} (map-indexed (fn [i r] [r (- n 1 i)]) order))
         :rails {} :thick 0.63 :color (or (:color b) "#2f6b4c")
         :x-min -2.2 :x-max (+ cols 1.2) :z-min -2 :z-max (+ n 1)})

      (some? (get sizes type))
      (let [mini (= type "mini")]
        {:kind "breadboard" :cols (or (:cols b) (get sizes type)) :row-order bb-row-order :rows bb-rows
         :rails (if mini {} rails) :thick 2.8 :color (or (:color b) "#f3efe4")
         :split (boolean (:split_rails b))
         :x-min -2.2 :x-max (+ (or (:cols b) (get sizes type)) 1.2)
         :z-min (if mini 1.2 -1.6) :z-max (if mini 15.8 18.6)})

      :else {:error (str "board " (q type) " is none of full, half, mini, perfboard, stripboard")})))

(defn hole
  "The hole named `k` (\"e12\", \"T+5\") on `board`: {:key :row|:rail :col
   :x :z}, or nil."
  [board k]
  (when (string? k)
    (let [k (str/trim k)]
      (if-let [m (re-matches #"([tTbB])([+-])([0-9]+)" k)]
        (let [rail (str (str/upper-case (nth m 1)) (nth m 2))
              col (digits (nth m 3))
              z (get (:rails board) rail)]
          (when (and (some? z) (<= 1 col (:cols board)))
            {:key (str rail col) :rail rail :col col :x (dec col) :z z}))
        (when-let [m (re-matches #"([a-zA-Z]+)([0-9]+)" k)]
          (let [row (str/lower-case (nth m 1))
                col (digits (nth m 2))
                z (get (:rows board) row)]
            (when (and (some? z) (<= 1 col (:cols board)))
              {:key (str row col) :row row :col col :x (dec col) :z z})))))))

(defn holes [board]
  (vec (for [prefix (concat (:row-order board) (keys (:rails board)))
             c (range 1 (inc (:cols board)))]
         (hole board (str prefix c)))))

(defn- prefix [h] (or (:rail h) (:row h)))

(defn line
  "The holes from `from` to `to` along a row or a column, both ends in;
   without `to`, `n` holes rightwards from `from`."
  [board from to n]
  (let [a (hole board from)]
    (cond
      (nil? a) []
      (nil? to) (vec (keep #(hole board (str (prefix a) (+ (:col a) %))) (range (max n 1))))
      :else
      (let [b (hole board to)]
        (cond
          (nil? b) [a]
          (= (prefix a) (prefix b))
          (let [step (if (>= (:col b) (:col a)) 1 -1)]
            (mapv #(hole board (str (prefix a) %)) (range (:col a) (+ (:col b) step) step)))
          (= (:col a) (:col b))
          (let [lo (min (:z a) (:z b)) hi (max (:z a) (:z b))
                hs (->> (concat (:row-order board) (keys (:rails board)))
                        (keep #(hole board (str % (:col a))))
                        (filter #(<= lo (:z %) hi))
                        (sort-by :z))]
            (vec (if (<= (:z a) (:z b)) hs (reverse hs))))
          :else [a b])))))

(defn node
  "What a hole is connected to inside the board: its 5-hole strip, its rail
   (half of it on a split full board), a stripboard row, or itself."
  [board h]
  (cond
    (:rail h) (str (:rail h) (if (and (:split board) (> (:col h) (/ (:cols board) 2))) "2" ""))
    (= "breadboard" (:kind board)) (str (if (< (:z h) 8.5) "U" "L") (:col h))
    (= "stripboard" (:kind board)) (str "S" (:row h))
    :else (:key h)))

;; ── Parts ──

(def ^:private two-pin
  {"resistor" ["1" "2"] "diode" ["anode" "cathode"] "led" ["anode" "cathode"]
   "capacitor" ["+" "-"] "button" ["1" "2"]})

(def part-types ["button" "capacitor" "diode" "dip" "header" "led" "module" "resistor"])

(defn- pins-of-rows [board p problems where]
  (let [rows (:pins p)]
    (if (and (sequential? rows) (seq rows))
      (reduce (fn [acc [ri r]]
                (let [names (vec (or (:names r) []))
                      hs (line board (:from r) (:to r) (count names))]
                  (when (or (empty? hs) (and (some? (:to r)) (nil? (hole board (:to r)))))
                    (swap! problems conj (str where ": pin row from " (q (:from r)) (when (:to r) (str " to " (q (:to r))))
                                             " is not on the board")))
                  (into acc (map-indexed (fn [i h] {:name (str (get names i "")) :hole h :row ri}) hs))))
              [] (map-indexed vector rows))
      (do (swap! problems conj (str where ": a module needs pins, rows of {from, to, names}"))
          []))))

(defn- dip-pins [board p problems where]
  (let [a (hole board (:at p))
        n0 (or (:pins p) 8)
        n (max 2 (- n0 (mod n0 2)))
        half (quot n 2)]
    (if-not a
      (do (swap! problems conj (str where ": at " (q (:at p)) " is not a hole on this board")) [])
      (let [other (or (:across p)
                      (if (= "breadboard" (:kind board))
                        (get across (:row a))
                        (letters (+ 3 (first (keep-indexed (fn [i r] (when (= r (:row a)) i)) (:row-order board)))))))
            b (hole board (str other (:col a)))]
        (if-not b
          (do (swap! problems conj (str where ": no row " (q other) " across from " (:key a))) [])
          (let [dir (if (> (:z a) (:z b)) 1 -1)
                names (vec (or (:names p) []))
                near (mapv (fn [i] {:name (str (get names i (inc i))) :hole (hole board (str (:row a) (+ (:col a) (* dir i)))) :row 0})
                           (range half))
                far (mapv (fn [i] {:name (str (get names (- n 1 i) (- n i))) :hole (hole board (str (:row b) (+ (:col a) (* dir i)))) :row 1})
                          (reverse (range half)))]
            (when (some #(nil? (:hole %)) near)
              (swap! problems conj (str where ": its " n " pins run off the board")))
            (filterv #(some? (:hole %)) (concat near far))))))))

(defn- end-hole [board p k problems where]
  (let [h (hole board (get p k))]
    (when-not h (swap! problems conj (str where ": " (kname k) " " (q (get p k)) " is not a hole on this board")))
    h))

(defn resolve-part
  "A layout part with its pins on `board`: {:key :id :type :label :spec
   :pins [{:name :hole :row}]}; `problems` (an atom of strings) collects
   what doesn't fit."
  [board p i problems]
  (let [type (:type p)
        where (or (:id p) (:label p) type "part")
        pins (cond
               (= type "module") (pins-of-rows board p problems where)
               (= type "dip") (dip-pins board p problems where)
               (= type "header") (let [names (vec (or (:names p) []))
                                       hs (line board (:from p) (:to p) (count names))]
                                   (when (empty? hs) (end-hole board p :from problems where))
                                   (vec (map-indexed (fn [i h] {:name (str (get names i "")) :hole h :row 0}) hs)))
               (some? (get two-pin type))
               (let [[k0 k1] (if (or (= type "led") (= type "diode")) [:anode :cathode] [:from :to])
                     k0 (if (some? (get p k0)) k0 :from)
                     k1 (if (some? (get p k1)) k1 :to)
                     a (end-hole board p k0 problems where)
                     b (end-hole board p k1 problems where)
                     [n0 n1] (get two-pin type)]
                 (vec (concat (when a [{:name n0 :hole a :row 0}]) (when b [{:name n1 :hole b :row 0}]))))
               :else (do (swap! problems conj (str where ": type " (q type) " is none of " (str/join ", " part-types)))
                         []))]
    {:key (or (:id p) (str "part-" (inc i))) :id (:id p) :type type
     :label (or (:label p) (:id p) "") :spec p :pins pins}))

(defn pin-of
  "The pin \"ID.PIN\" names among `parts`: by name (any case), or by number
   when no pin is named that."
  [parts ref]
  (let [ref (str ref)
        dot (str/index-of ref ".")]
    (when (and dot (pos? dot))
      (let [id (subs ref 0 dot)
            nm (subs ref (inc dot))
            part (some #(when (= id (:id %)) %) parts)]
        (when part
          (or (some #(when (= (str/lower-case nm) (str/lower-case (:name %))) %) (:pins part))
              (when (re-matches #"[0-9]+" nm)
                (let [n (digits nm)]
                  (when (<= 1 n (count (:pins part))) (nth (:pins part) (dec n)))))))))))

;; ── Wires and nets ──

(defn- resolve-end [board parts e where problems]
  (cond
    (map? e) {:off (str (or (:label e) "?")) :side (:side e)}
    :else
    (let [h (hole board e)]
      (cond
        h {:hole h}
        (pin-of parts e) {:hole (:hole (pin-of parts e)) :ref (str e)}
        :else (do (swap! problems conj (str where ": " (q e) " is neither a hole, a part pin (ID.PIN) nor {label}"))
                  nil)))))

(defn- root-of [parent k]
  (loop [k k]
    (let [p (get parent k)]
      (if (and (some? p) (not= p k)) (recur p) k))))

(defn end-node [board e]
  (if (:off e) (str "off:" (:off e)) (node board (:hole e))))

(defn resolve-layout
  "A layout drawn out: {:board :parts :wires :roots :problems}. Wires are
   {:key :id :label :spec :from :to}; an end is {:hole} (with :ref when named
   by a part pin) or {:off label :side}, off-board ends always :to. :roots
   maps each wired node to its net."
  [layout]
  (let [problems (atom [])
        board (board (:board layout))]
    (if (:error board)
      {:board board :parts [] :wires [] :roots {} :problems [(:error board)]}
      (let [parts (vec (map-indexed (fn [i p] (resolve-part board p i problems))
                                    (if (sequential? (:parts layout)) (:parts layout) [])))
            wires (vec (keep-indexed
                        (fn [i w]
                          (let [where (str "wire " (or (:id w) (str "#" (inc i))))
                                a (resolve-end board parts (:from w) (str where " from") problems)
                                b (resolve-end board parts (:to w) (str where " to") problems)]
                            (cond
                              (or (nil? a) (nil? b)) nil
                              (and (:off a) (:off b)) (do (swap! problems conj (str where ": both ends are off the board")) nil)
                              :else {:key (or (:id w) (str "wire-" (inc i))) :id (:id w) :label (or (:label w) "")
                                     :spec w :from (if (:off a) b a) :to (if (:off a) a b)})))
                        (if (sequential? (:wires layout)) (:wires layout) [])))
            parent (reduce (fn [parent w]
                             (let [ra (root-of parent (end-node board (:from w)))
                                   rb (root-of parent (end-node board (:to w)))]
                               (if (= ra rb) parent (assoc parent ra rb))))
                           {} wires)
            roots (into {} (map (fn [k] [k (root-of parent k)]) (keys parent)))]
        {:board board :parts parts :wires wires :roots roots :problems @problems}))))

(defn net-of
  "The net an end ({:hole} or {:off}) is on, in a `resolve-layout`ed layout."
  [r e]
  (let [n (end-node (:board r) e)]
    (or (get (:roots r) n) n)))

;; ── Checking a layout ──

(defn targets
  "A list of ids as views take them: a vector, or one string of ids
   separated by spaces."
  [v]
  (if (sequential? v)
    (mapv str v)
    (vec (remove str/blank? (str/split (str (or v "")) #" ")))))

(defn target?
  "Does `t` name something in `r`: a part, a wire, a hole or a part pin?"
  [r t]
  (boolean (or (some #(= t (:key %)) (:parts r))
               (some #(= t (:key %)) (:wires r))
               (hole (:board r) t)
               (pin-of (:parts r) t))))

(defn problems
  "What is wrong with `layout`, as messages for the agent; empty when it
   draws as meant."
  [layout]
  (let [r (resolve-layout layout)
        ids (keep :id (if (sequential? (:parts layout)) (:parts layout) []))
        dups (keep (fn [id] (let [n (count (filter #(= id %) ids))]
                              (when (> n 1) (str "part id " (q id) " is used " n " times"))))
                   (distinct ids))
        steps (:steps layout)
        step-problems (cond
                        (nil? steps) []
                        (not (sequential? steps)) ["steps: a list of {title, text, add, highlight, focus, view, …}"]
                        :else (for [[i s] (map-indexed vector steps)
                                    k [:add :highlight :focus :only :show :hide]
                                    t (targets (get s k))
                                    :when (not (target? r t))]
                                (str "step " (inc i) " " (kname k) ": " (q t) " is no part, wire, hole or part pin")))]
    (vec (take 20 (concat dups (:problems r) step-problems)))))
