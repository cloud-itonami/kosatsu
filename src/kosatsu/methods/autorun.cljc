(ns kosatsu.methods.autorun
  "autorun.py — kosatsu (高札) AUTONOMOUS crime/sanctions competing-claim heartbeat on the kotoba
  Datom log. ADR-2606072000. 1:1 Clojure port of `src/kosatsu/methods/autorun.cljc`.

  Each heartbeat the actor runs its whole competing-claim pipeline ITSELF, no human in the loop:

    observe (load the OFFLINE designation-graph seed) → weave (validate every authority / subject /
      designation event against the gates; raises on a violation)
      → report (aggregate, politically-neutral: agreement index, per-subject divergence
        {contested | unanimous | single-asserter}, by-authority coverage, co-designation)
      → PERSIST a content-addressed transaction to the append-only kotoba Datom log
        (graph datoms + derived :kosatsu.div/* signals), linking the previous tx's CID.

  Constitutional posture holds by construction: etzhayyim authors NO designation (every designation
  carries its own :asserter — a sovereign/body, never etzhayyim), NO verdict, NO per-subject score.

  The loop is deterministic / resume-safe: `canonical-order` sorts datoms by canonical JSON before
  hashing so the CID is reproducible across processes regardless of any set-iteration order inside
  report. Append-only. WHAT STAYS GATED (G7/G8): no live designation-list ingest, no live-node push,
  no live posting (posts are dry-run, owned by social.cljc).

  House style: requires only the GOOD sibling .cljc ports (edn + weave + kotoba), not any stub.
  (The Python `__main__` argparse demo printer is preserved behind #?(:clj …) as -main.)"
  (:require [kosatsu.methods.edn :as edn]
            [kosatsu.methods.weave :as weave]
            [kosatsu.methods.kotoba :as kotoba]
            #?(:clj [clojure.java.io :as io])
            [clojure.string :as str]))

(def base-as-of 20260609)

#?(:clj (def here (io/file (or (System/getProperty "user.dir") "."))))
#?(:clj (def data (io/file here "data")))
#?(:clj (def seed-default (when data (io/file data "seed-designation-graph.kotoba.edn"))))
#?(:clj (def log-default (when data (io/file data "kosatsu.datoms.kotoba.edn"))))

;; ── canonical JSON for the deterministic datom sort key ──────────────────────
;; Mirror of json.dumps(d, ensure_ascii=False, sort_keys=True) over one datom (the sort KEY only).
(defn- json-escape-utf8 ^String [^String s]
  (str/escape s {\" "\\\"" \\ "\\\\"
                 \backspace "\\b" \tab "\\t" \newline "\\n" \formfeed "\\f" \return "\\r"}))

(defn- json-key ^String [v]
  (cond
    (string? v)     (str "\"" (json-escape-utf8 v) "\"")
    (boolean? v)    (if v "true" "false")
    (nil? v)        "null"
    (integer? v)    (str v)
    (number? v)     (str v)
    (map? v)        (str "{" (str/join ", " (map (fn [k] (str "\"" (json-escape-utf8 (str k)) "\": "
                                                              (json-key (get v k))))
                                                 (sort (keys v)))) "}")
    (sequential? v) (str "[" (str/join ", " (map json-key v)) "]")
    :else (str v)))

(defn canonical-order
  "Sort datoms by canonical JSON so the tx is DETERMINISTIC regardless of any set-iteration order
  inside report. EAVT is an unordered set, so a canonical sort makes the content-addressed CID
  reproducible / resume-safe. Mirrors _canonical_order."
  [datoms]
  (vec (sort-by json-key datoms)))

#?(:clj
   (defn run-cycle
     "One autonomous heartbeat: observe → weave (validate) → report → persist a content-addressed
     Datom transaction (graph + derived :kosatsu.div/* signals). cycle drives tx-id + as-of."
     ([cycle] (run-cycle cycle seed-default log-default))
     ([cycle seed-path log-path]
      (let [g (weave/weave (edn/load-edn seed-path))   ; observe + VALIDATE (raises on any gate)
            r (weave/report g)                          ; aggregate, politically-neutral (G2/G4/G9)
            datoms (canonical-order (into (kotoba/graph-datoms g) (kotoba/derived-datoms r)))
            tx (kotoba/make-tx datoms :tx-id cycle :as-of (+ base-as-of cycle)
                               :prev-cid (kotoba/head-cid log-path))
            cid (kotoba/append-tx tx log-path)          ; PERSIST to append-only LOCAL kotoba log
            ai (get r "agreement_index")]
        {"cycle" cycle
         "authorities" (get r "authority_count")
         "subjects" (get r "subject_count")
         "designations" (get r "designation_count")
         "contested" (get ai "contested")
         "unanimous" (get ai "unanimous")
         "single_asserter" (get ai "single_asserter")
         "datoms" (count datoms)
         "cid" cid}))))

#?(:clj
   (defn run-autonomous
     ([] (run-autonomous 3 seed-default log-default))
     ([cycles] (run-autonomous cycles seed-default log-default))
     ([cycles seed-path log-path]
      (let [beats (mapv #(run-cycle % seed-path log-path) (range 1 (inc cycles)))]
        {"cycles" cycles
         "beats" beats
         "log_length" (count (kotoba/read-log log-path))
         "head_cid" (kotoba/head-cid log-path)
         "chain" (kotoba/verify-chain log-path)}))))

#?(:clj
   (defn -main
     "CLI entry: run N autonomous heartbeats → LOCAL kotoba Datom log. --cycles/--seed/--log/--fresh
     (file I/O at the edge)."
     [& argv]
     (let [argv (vec argv)
           arg-after (fn [flag dflt] (let [i (.indexOf argv flag)]
                                       (if (>= i 0) (nth argv (inc i)) dflt)))
           cycles (let [v (arg-after "--cycles" nil)] (if v (Long/parseLong v) 3))
           seed-path (io/file (arg-after "--seed" (str seed-default)))
           log-path (io/file (arg-after "--log" (str log-default)))]
       (when (and (some #{"--fresh"} argv) (.exists log-path)) (.delete log-path))
       (let [res (run-autonomous cycles seed-path log-path)]
         (println (str "# kosatsu (高札) — AUTONOMOUS competing-claim over the kotoba Datom log "
                       "(offline seed, LOCAL persist; live ingest / posting stays G7/G8-gated)\n"))
         (doseq [bt (get res "beats")]
           (println (str "  ♥ cycle " (get bt "cycle") ": " (get bt "authorities") " authorities / "
                         (get bt "subjects") " subjects / " (get bt "designations") " designations · contested "
                         (get bt "contested") " · unanimous " (get bt "unanimous") " · single-asserter "
                         (get bt "single_asserter") " +" (get bt "datoms") " datoms → cid "
                         (subs (get bt "cid") 0 14) "…")))
         (let [ch (get res "chain")]
           (println (str "\n  log: " (get res "log_length") " tx · head "
                         (subs (get res "head_cid") 0 14) "… · chain "
                         (if (get ch "ok") "OK ✓" (str "BROKEN at " (get ch "broken_at")))
                         " · every designation ATTRIBUTED; no designation/verdict/score authored by etzhayyim (G2/G4)")))))))
