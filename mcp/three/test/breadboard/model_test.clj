(ns breadboard.model-test
  (:require [breadboard.model :as m]
            [clojure.test :refer [deftest is testing]]))

(def nano
  {:type "module" :id "NANO"
   :pins [{:from "h1" :to "h15" :names ["D12" "D11" "D10" "D9" "D8" "D7" "D6" "D5" "D4" "D3" "D2" "GND" "RST" "RX0" "TX1"]}
          {:from "d1" :to "d15" :names ["D13" "3V3" "REF" "A0" "A1" "A2" "A3" "A4" "A5" "A6" "A7" "5V" "RST" "GND" "VIN"]}]})

(def layout
  {:board "half"
   :parts [nano {:type "resistor" :id "R" :from "i10" :to "i16" :ohms 1000}]
   :wires [{:id "P1" :from "a12" :to "B+12"}
           {:id "P3" :from "T+30" :to "B+30"}
           {:id "S" :from "NANO.D10" :to {:label "RC522 SDA"}}]})

(deftest holes
  (let [b (m/board "half")]
    (is (= {:key "e12" :row "e" :col 12 :x 11 :z 10} (m/hole b "E12")))
    (is (= {:key "T+5" :rail "T+" :col 5 :x 4 :z 1} (m/hole b "t+5")))
    (is (nil? (m/hole b "k1")) "no row k on a breadboard")
    (is (nil? (m/hole b "a31")) "a half board has 30 columns")
    (is (nil? (m/hole (m/board "mini") "T+1")) "a mini board has no rails"))
  (testing "perfboard rows count from the near edge"
    (let [b (m/board {:type "perfboard" :cols 10 :rows 4})]
      (is (= 3 (:z (m/hole b "a1"))))
      (is (= 0 (:z (m/hole b "d1"))))
      (is (nil? (m/hole b "e1"))))))

(deftest strips-and-nets
  (let [r (m/resolve-layout layout)
        b (:board r)]
    (is (= "L12" (m/node b (m/hole b "a12"))) "a-e of a column are one strip")
    (is (= "U12" (m/node b (m/hole b "j12"))))
    (is (= (m/net-of r {:hole (m/hole b "d12")})
           (m/net-of r {:hole (m/hole b "T+3")}))
        "the Nano's 5V reaches the top rail through P1, the bottom rail and P3")
    (is (not= (m/net-of r {:hole (m/hole b "d12")})
              (m/net-of r {:hole (m/hole b "d13")})))
    (is (= (m/net-of r {:hole (m/hole b "h3")}) (m/net-of r {:off "RC522 SDA"}))
        "a wire to an off-board label joins its net")))

(deftest pins
  (let [r (m/resolve-layout layout)]
    (is (= "h3" (:key (:hole (m/pin-of (:parts r) "NANO.d10")))) "by name, any case")
    (is (= "h2" (:key (:hole (m/pin-of (:parts r) "NANO.2")))) "by number")
    (is (nil? (m/pin-of (:parts r) "NANO.FOO")))
    (is (= ["1" "2"] (mapv :name (:pins (second (:parts r))))))))

(deftest dip
  (let [r (m/resolve-layout {:board "half" :parts [{:type "dip" :id "U" :at "e10" :pins 8}]})
        pins (:pins (first (:parts r)))]
    (is (= ["e10" "e11" "e12" "e13" "f13" "f12" "f11" "f10"] (mapv #(:key (:hole %)) pins))
        "pin 1 bottom left, counting round")
    (is (= ["1" "8"] [(:name (first pins)) (:name (last pins))]))))

(deftest problems
  (is (= [] (m/problems layout)))
  (is (= ["part id \"N\" is used 2 times"
          "N: pin row from \"k1\" is not on the board"
          "N: to \"a40\" is not a hole on this board"
          "wire W from: \"N.FOO\" is neither a hole, a part pin (ID.PIN) nor {label}"
          "step 1 add: \"NOPE\" is no part, wire, hole or part pin"]
         (m/problems {:board "half"
                      :parts [{:type "module" :id "N" :pins [{:from "k1" :names ["A"]}]}
                              {:type "resistor" :id "N" :from "a1" :to "a40"}]
                      :wires [{:id "W" :from "N.FOO" :to "a2"}]
                      :steps [{:add ["NOPE"]} {:highlight "a1 B+3"}]})))
  (is (= ["board \"giant\" is none of full, half, mini, perfboard, stripboard"]
         (m/problems {:board "giant"}))))
