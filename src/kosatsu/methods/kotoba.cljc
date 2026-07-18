(ns kosatsu.methods.kotoba
  "kotoba.py — kosatsu (高札) kotoba Datom-log writer (local, content-addressed).
  ADR-2606072000 + ADR-2605262130 + ADR-2605312345. 1:1 Clojure port of `src/kosatsu/methods/kotoba.cljc`.

  The substrate boundary (root CLAUDE.md): canonical state is the kotoba Datom log —
  content-addressed EAVT assertions, append-only (非終末論; every designation is an append-only
  ATTRIBUTED event — asserter + as-of :listed/:delisted — never overwritten). This module is the
  local, autonomous-loop write path — the same path the infra-intel/observatory family uses
  (shionome / danjo / keizu …): a self-driving heartbeat appends content-addressed transactions to
  a local append-only EDN log with NO external I/O.

  Constitutional posture is preserved by construction (kosatsu hard rules): etzhayyim authors NO
  designation (every designation carries an :asserter — a sovereign/body, never etzhayyim), NO
  verdict, NO per-subject score. The computed divergence {contested | unanimous | single-asserter}
  makes \"crime varies by political stance\" a NEUTRAL fact.

    - graph-datoms(g)   → EAVT assertions for every entity (authority / subject / designation event).
    - derived-datoms(r) → EAVT assertions for the aggregate competing-claim signals, flagged
                          :kosatsu.div/derived. Never a per-subject score / verdict.
    - make-tx / append-tx / read-log / head-cid / verify-chain — content-addressed commit-DAG.

  EAVT = [op entity attribute value]; op is :db/add only (append-only — no :db/retract).
  Deterministic: the caller supplies tx-id + as-of (no wall clock) → resume-safe.

  House style (mirrors danjo.methods.kotoba): map keys stay verbatim string keys, Python ':ns/name'
  keyword strings stay literal strings; pure fns; file I/O only behind #?(:clj …). SELF-CONTAINED:
  own sha-256 + canonical JSON; the read-back EDN reader reuses the sibling kosatsu.methods.edn
  (the real port of _edn.py). The tx CID reproduces Python `'b' + hashlib.sha256(json.dumps(
  {'prev':…,'datoms':…}, ensure_ascii=False, sort_keys=True, separators=(',',':')))` byte-for-byte.
  (The Python `__main__` demo printer is omitted — note it.)"
  (:require [clojure.string :as str]
            [kosatsu.methods.edn :as edn]))

;; ── sha-256 host seam ─────────────────────────────────────────────────────────
(def ^:dynamic *sha256-hex*
  "String → lowercase hex sha-256 digest (UTF-8). Rebind on hosts without MessageDigest."
  #?(:clj (fn [^String s]
            (let [d (.digest (java.security.MessageDigest/getInstance "SHA-256")
                             (.getBytes s "UTF-8"))]
              (str/join (map #(let [h (Integer/toHexString (bit-and % 0xff))]
                                (if (= 1 (count h)) (str "0" h) h))
                             d))))
     :default (fn [_] (throw (ex-info "bind kosatsu.methods.kotoba/*sha256-hex* on this host" {})))))

(def id-keys [":authority/id" ":subject/id" ":designation/id"])

;; ── EAVT assertion ────────────────────────────────────────────────────────────
(defn add
  "One append-only EAVT assertion: [:db/add <entity> <attr> <value>]."
  [entity attr value]
  [":db/add" entity attr value])

(defn- flatten-rows
  "Mirror of _flatten: for each map row, find its id (first present ID_KEY), then assert one datom
  per (non-id) attr; list values fan out one datom per item. Iteration follows insertion order."
  [rows]
  (let [items (if (map? rows) (vals rows) rows)]
    (reduce
     (fn [out row]
       (if-not (map? row)
         out
         (let [e (some (fn [k] (when (contains? row k) (get row k))) id-keys)]
           (if (nil? e)
             out
             (reduce
              (fn [out k]
                (if (some #{k} id-keys)
                  out
                  (let [v (get row k)
                        items (if (sequential? v) v [v])]
                    (reduce (fn [out item] (conj out (add e k item))) out items))))
              out
              (keys row))))))
     []
     items)))

(defn graph-datoms
  "Flatten the woven competing-claim graph into append-only EAVT assertions. Each designation is an
  ATTRIBUTED event (it carries its own :asserter — etzhayyim authors none)."
  [g]
  (into (into (flatten-rows (get g "authorities"))
              (flatten-rows (get g "subjects")))
        (flatten-rows (get g "designations"))))

(defn- lstrip-colon [^String s]
  (loop [i 0] (if (and (< i (count s)) (= \: (nth s i))) (recur (inc i)) (subs s i))))

(defn derived-datoms
  "Flatten the aggregate competing-claim report into EAVT assertions, each flagged
  :kosatsu.div/derived true (a politically-neutral observation recomputed on read — NEVER a verdict
  or a per-subject score). `r` is report()."
  ([r] (derived-datoms r "kosatsu.div"))
  ([r prefix]
   (let [ai (get r "agreement_index")
         e  (str prefix "-agreement")
         out (transient
              (into []
                    [(add e ":kosatsu.div/authority-count" (get r "authority_count"))
                     (add e ":kosatsu.div/subject-count" (get r "subject_count"))
                     (add e ":kosatsu.div/designation-count" (get r "designation_count"))
                     (add e ":kosatsu.div/contested" (get ai "contested"))
                     (add e ":kosatsu.div/single-asserter" (get ai "single_asserter"))
                     (add e ":kosatsu.div/unanimous" (get ai "unanimous"))
                     (add e ":kosatsu.div/contested-ratio" (get ai "contested_ratio"))
                     (add e ":kosatsu.div/derived" true)]))
         out (reduce
              (fn [out d]
                (let [es (str prefix "-subject-" (get d "subject"))]
                  (-> out
                      (conj! (add es ":kosatsu.div/subject" (get d "subject")))
                      (conj! (add es ":kosatsu.div/class" (str ":" (lstrip-colon (str (get d "class"))))))
                      (conj! (add es ":kosatsu.div/listing" (get d "listing")))
                      (conj! (add es ":kosatsu.div/delisted" (get d "delisted")))
                      (conj! (add es ":kosatsu.div/silent" (get d "silent")))
                      (conj! (add es ":kosatsu.div/derived" true)))))
              out
              (get r "divergence"))
         out (reduce
              (fn [out a]
                (let [ea (str prefix "-authority-" (get a "authority"))]
                  (-> out
                      (conj! (add ea ":kosatsu.div/authority" (get a "authority")))
                      (conj! (add ea ":kosatsu.div/jurisdiction" (get a "jurisdiction")))
                      (conj! (add ea ":kosatsu.div/listed-subjects" (get a "listed_subjects")))
                      (conj! (add ea ":kosatsu.div/derived" true)))))
              out
              (get r "by_authority"))
         out (reduce
              (fn [out [i c]]
                (let [ec (str prefix "-codesig-" i)]
                  (-> out
                      (conj! (add ec ":kosatsu.div/asserter" (get c "asserter")))
                      (conj! (add ec ":kosatsu.div/program" (get c "program")))
                      (conj! (add ec ":kosatsu.div/co-designation-count" (get c "count")))
                      (conj! (add ec ":kosatsu.div/derived" true)))))
              out
              (map-indexed vector (get r "co_designation")))]
     (persistent! out))))

;; ── canonical JSON for the CID preimage ──────────────────────────────────────
;; Mirrors _canonical: json.dumps({"prev":…,"datoms":…}, ensure_ascii=False, sort_keys=True,
;; separators=(",",":")). ensure_ascii=FALSE → non-ASCII emitted RAW, not \uXXXX.
(defn- json-escape-utf8 ^String [^String s]
  (str/escape s {\" "\\\"" \\ "\\\\"
                 \backspace "\\b" \tab "\\t" \newline "\\n" \formfeed "\\f" \return "\\r"}))

(defn- canonical-json-utf8 ^String [v]
  (cond
    (string? v)     (str "\"" (json-escape-utf8 v) "\"")
    (boolean? v)    (if v "true" "false")
    (nil? v)        "null"
    (integer? v)    (str v)
    (number? v)     (str v)
    (map? v)        (str "{" (str/join "," (map (fn [k] (str "\"" (json-escape-utf8 (str k)) "\":"
                                                             (canonical-json-utf8 (get v k))))
                                                (sort (keys v)))) "}")
    (sequential? v) (str "[" (str/join "," (map canonical-json-utf8 v)) "]")
    :else (throw (ex-info "canonical-json-utf8: unsupported value" {:value v}))))

(defn- canonical [datoms prev-cid]
  ;; sort_keys=True orders the top map's keys: "datoms" < "prev".
  (canonical-json-utf8 {"prev" prev-cid "datoms" datoms}))

(defn tx-cid
  "Content address = sha256 over (prev-cid, datoms) → a commit-DAG."
  ([datoms] (tx-cid datoms ""))
  ([datoms prev-cid]
   (str "b" (*sha256-hex* (canonical datoms prev-cid)))))

(defn make-tx
  "Assemble one content-addressed transaction map (string :tx/* keys, mirrors Python)."
  [datoms & {:keys [tx-id as-of prev-cid] :or {prev-cid ""}}]
  {":tx/id"     tx-id
   ":tx/as-of"  as-of
   ":tx/prev"   prev-cid
   ":tx/cid"    (tx-cid datoms prev-cid)
   ":tx/count"  (count datoms)
   ":tx/datoms" datoms})

;; ── EDN value rendering (mirrors _edn_val) ───────────────────────────────────
(defn- json-dumps-str
  "json.dumps(s, ensure_ascii=False) — a double-quoted, escaped JSON string."
  [^String s]
  (str "\"" (json-escape-utf8 s) "\""))

(defn- edn-val ^String [v]
  (cond
    (boolean? v) (if v "true" "false")
    (integer? v) (str v)
    (number? v)  (str v)
    (string? v)  (if (str/starts-with? v ":") v (json-dumps-str v))
    (sequential? v) (str "[" (str/join " " (map edn-val v)) "]")
    :else (json-dumps-str (str v))))

(defn- tx-to-edn ^String [tx]
  (let [datoms (str/join " " (map (fn [d] (str "[" (str/join " " (map edn-val d)) "]"))
                                  (get tx ":tx/datoms")))]
    (str "{:tx/id " (get tx ":tx/id")
         " :tx/as-of " (get tx ":tx/as-of")
         " :tx/prev " (json-dumps-str (get tx ":tx/prev"))
         " :tx/cid " (json-dumps-str (get tx ":tx/cid"))
         " :tx/count " (get tx ":tx/count")
         " :tx/datoms [" datoms "]}")))

(def ^:private log-header
  (str ";; kosatsu kotoba Datom log — append-only EAVT transactions "
       "(content-addressed DAG). Every designation is an ATTRIBUTED event; "
       "etzhayyim authors no designation, no verdict, no score. ADR-2606072000.\n"))

#?(:clj
   (defn append-tx
     "Append ONE transaction to the append-only log (never rewrites). Returns the tx CID."
     [tx log-path]
     (let [f (clojure.java.io/file (str log-path))]
       (when-let [parent (.getParentFile f)] (.mkdirs parent))
       (when-not (.exists f) (spit f log-header))
       (spit f (str (tx-to-edn tx) "\n") :append true)
       (get tx ":tx/cid"))))

#?(:clj
   (defn read-log
     "Read the log back as a vector of transaction maps (uses the shared kosatsu.methods.edn reader).
     Skips blank + ;-comment lines."
     [log-path]
     (let [f (clojure.java.io/file (str log-path))]
       (if-not (.exists f)
         []
         (->> (str/split-lines (slurp f))
              (map str/trim)
              (remove (fn [l] (or (str/blank? l) (str/starts-with? l ";"))))
              (mapv edn/parse-edn))))))

#?(:clj
   (defn head-cid
     "The CID of the last tx in the log (\"\" if empty)."
     [log-path]
     (let [txs (read-log log-path)]
       (if (seq txs) (get (last txs) ":tx/cid") ""))))

#?(:clj
   (defn verify-chain
     "Recompute every CID from its datoms + prev; verify the DAG is intact. {ok length broken-at}."
     [log-path]
     (let [txs (read-log log-path)]
       (loop [i 0, prev "", ts txs]
         (if (empty? ts)
           {"ok" true "length" (count txs) "broken_at" -1}
           (let [tx (first ts)
                 expect (tx-cid (get tx ":tx/datoms" []) prev)]
             (if (or (not= (get tx ":tx/cid") expect) (not= (get tx ":tx/prev") prev))
               {"ok" false "length" (count txs) "broken_at" i}
               (recur (inc i) (get tx ":tx/cid") (rest ts)))))))))
