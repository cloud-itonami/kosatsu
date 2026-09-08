(ns kosatsu.methods.bridge
  "bridge.py — 高札 (kosatsu) cross-actor SoS composition. ADR-2606072000.
  1:1 Clojure port of `src/kosatsu/methods/bridge.cljc`.

  kosatsu is the competing-claim BOARD; the SoS intel value comes from composing its divergence
  view with the sibling accountability/intel actors over the SHARED kotoba Datom log. This module
  computes the JOIN KEYS kosatsu exposes so a downstream actor can link a designation subject to
  its other observations — WITHOUT kosatsu reaching into another actor's graph (each actor owns its
  own datoms; bridge only emits the keys).

    - tadori 辿   : a :designated-wallet subject → on-chain attribution case (authorized only)
    - keizu 系図  : a :designated-org/entity subject → public power-relations node (if public role)
    - tsumugi 紡ぎ: a subject's asserter → power-entity 縁 (who designates whom, as influence)
    - kanae 鼎    : the by-authority / divergence aggregates → fiscal/atlas render
    - tasuke 助   : never — a victim-support flow is person-consented, disjoint (no auto-link)

  The bridge is MAP-ONLY (G9): it surfaces a join key for resilience/awareness/due-process, never an
  enforcement instruction. It NEVER promotes a contested designation to a verdict.

  House style: Python ':…' keyword strings stay literal strings; pure fns. (The Python `__main__`
  demo printer is omitted — note it.)"
  (:require [kotoba.lang.text :as str]
            [kosatsu.methods.weave :as weave]))

;; subject kind → the sibling actor that can further observe it (join target). Person/org subjects
;; only bridge to a PUBLIC-role actor when they are already a public power role (keizu's own G1
;; decides admissibility downstream); kosatsu only emits the key, never the link.
(def bridge-targets
  {"designated-wallet" "tadori"
   "designated-domain" "tadori"
   "designated-org" "keizu"
   "designated-entity" "keizu"})

(defn- lstrip-colon [^String s]
  (loop [i 0] (if (and (< i (count s)) (= \: (nth s i))) (recur (inc i)) (subs s i))))

(defn join-keys
  "For each currently-LISTED-by-someone subject, emit a cross-actor join key + its divergence class.
  Delisted-everywhere / silent subjects are skipped (nothing live to compose). The key is advisory
  routing only (G9). Subjects iterate in insertion order, then the result is sorted."
  ([g] (join-keys g nil))
  ([g ts]
   (let [out (reduce
              (fn [out [sid s]]
                (let [div (weave/divergence g sid ts)]
                  (if (empty? (get div "listing"))   ; nobody currently lists it → no live composition
                    out
                    (let [kind (lstrip-colon (str (get s ":subject/kind" "")))
                          target (get bridge-targets kind)]
                      (conj out {"subject" sid
                                 "subject_kind" kind
                                 "divergence_class" (get div "class")
                                 "listing_asserters" (get div "listing")
                                 "bridge_to" target
                                 "note" "advisory join key for SoS awareness; never an enforcement instruction (G9)"})))))
              []
              (get g "subjects"))]
     (vec (sort-by (fn [x] [(or (get x "bridge_to") "~") (get x "subject")]) out)))))

(defn tsumugi-en-edges
  "tsumugi 縁 projection: each currently-listed designation is an asserter→subject INFLUENCE edge
  (who exercises designating power over whom). Edge-primary, attributed; never a per-node score.
  Feeds tsumugi's power-entity 縁 weave."
  ([g] (tsumugi-en-edges g nil))
  ([g ts]
   (let [out (reduce
              (fn [out d]
                (let [sub (get d ":designation/subject")
                      ass (get d ":designation/asserter")]
                  (if (= (weave/status-as-of g sub ass ts) "listed")
                    (conj out {"from" ass "to" sub "kind" "designation-power"
                               "measure" (lstrip-colon (str (get d ":designation/measure" "")))
                               "as_of" (get d ":designation/posted-at")})
                    out)))
              []
              (get g "designations"))]
     (vec (sort-by (fn [x] [(get x "from") (get x "to")]) out)))))
